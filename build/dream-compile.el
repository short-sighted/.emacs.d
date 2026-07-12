;;; dream-compile.el --- Ahead-of-time build for Dream Emacs. -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dream-paths)
(require 'dream-startup)

(cl-eval-when (compile)
  (require 'borg)
  (require 'dream-autoloads-build))

(setenv "LSP_USE_PLISTS" "true")
(startup-redirect-eln-cache dream-eln-directory)

(defconst dream-build-core-files
  '("core/dream-paths.el"
    "core/dream-lib.el"
    "core/dream-hooks.el"
    "core/dream-startup.el"
    "core/dream-autoloads.el"
    "core/dream-setup.el"))

(defconst dream-build-tool-files
  '("build/dream-autoloads-build.el"
    "build/dream-compile.el"))

(defconst dream-build-default-config-native nil
  "Whether Borg full rebuilds should native-compile owned config.")

(defun dream-build-tool-files ()
  "Return absolute build-only configuration files."
  (mapcar (lambda (file) (expand-file-name file user-emacs-directory))
          dream-build-tool-files))

(defun dream-build--scan-tree (name regexp)
  "Return owned files below NAME whose basenames match REGEXP."
  (let ((directory (expand-file-name name user-emacs-directory)))
    (when (file-directory-p directory)
      (directory-files-recursively
       directory regexp nil #'dream-paths-searchable-subdirectory-p))))

(defun dream-build--assert-unique-library-names (files)
  "Signal when two FILES would shadow each other in `load-path'."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (file files)
      (let* ((name (file-name-base file))
             (previous (gethash name seen)))
        (when previous
          (error "Owned library basename %s is duplicated by %s and %s"
                 name previous file))
        (puthash name file seen))))
  files)

(defun dream-build-owned-lisp-files ()
  "Return sorted, uniquely named owned libraries under lib/ and lisp/."
  (dream-build--assert-unique-library-names
   (sort (append
          (dream-build--scan-tree "lib" "\\`dream-[[:alnum:]].*\\.el\\'")
          (dream-build--scan-tree "lisp" "\\`init-[[:alnum:]].*\\.el\\'"))
         #'string<)))

(defun dream-build-config-files ()
  "Return owned runtime files in deterministic compilation order."
  (let* ((core (mapcar (lambda (file)
                         (expand-file-name file user-emacs-directory))
                       dream-build-core-files))
         (owned (dream-build-owned-lisp-files))
         (init (expand-file-name "init.el" user-emacs-directory)))
    (dream-build--assert-unique-library-names
     (append core owned (list init) (dream-build-tool-files)))
    (append core owned (list init))))

(defun dream-build--source-entry (file)
  "Return the manifest entry for source FILE."
  (list (file-relative-name file user-emacs-directory)
        (dream-startup--file-hash file)))

(defun dream-build-manifest (&optional config-native packages-native)
  "Return a manifest for CONFIG-NATIVE and PACKAGES-NATIVE artifacts."
  (list :schema dream-build-manifest-schema
        :emacs-version emacs-version
        :native-version (dream-startup-native-version)
        :eln-directory (file-relative-name dream-eln-directory
                                           user-emacs-directory)
        :system-configuration system-configuration
        :lsp-use-plists (getenv "LSP_USE_PLISTS")
        ;; :native is retained as a manifest-format alias for config artifacts.
        :native (and config-native t)
        :config-native (and config-native t)
        :packages-native (and packages-native t)
        :generated-at (current-time)
        :sources (mapcar #'dream-build--source-entry
                         (append (dream-build-config-files)
                                 (dream-build-tool-files)))))

(defun dream-build-write-manifest (&optional config-native packages-native)
  "Atomically write the build manifest for the two native build modes."
  (make-directory (file-name-directory dream-build-manifest-file) t)
  (let ((temporary (make-temp-file
                    (expand-file-name ".manifest-" dream-cache-directory))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 (dream-build-manifest config-native packages-native)
                     (current-buffer))
              (insert "\n")))
          (rename-file temporary dream-build-manifest-file t))
      (when (file-exists-p temporary)
        (delete-file temporary))))
  dream-build-manifest-file)

(defun dream-build--compile-file (file &optional native strict)
  "Compile FILE, using synchronous native compilation when NATIVE is non-nil.
When STRICT is non-nil, turn byte compiler warnings into errors."
  (message "\n--- [Dream %s] ---" (file-relative-name file user-emacs-directory))
  (let ((byte-compile-error-on-warn strict)
        (byte-compile-warnings t))
    (if native
        (progn
          (require 'borg)
          (or (borg-byte+native-compile file)
              (error "Native compilation failed for %s" file)))
      (or (byte-compile-file file)
          (error "Byte compilation failed for %s" file)))))

(defun dream-build--verify-config-artifacts (&optional native)
  "Verify bytecode and, when NATIVE is non-nil, native config artifacts."
  (dolist (file (dream-build-config-files))
    (let ((bytecode (concat (file-name-sans-extension file) ".elc")))
      (unless (file-readable-p bytecode)
        (error "Missing bytecode artifact: %s" bytecode)))
    (when native
      (let ((eln (comp-el-to-eln-filename file)))
        (unless (file-readable-p eln)
          (error "Missing native artifact: %s" eln)))))
  (dolist (file (dream-build-tool-files))
    (let ((bytecode (concat (file-name-sans-extension file) ".elc")))
      (unless (file-readable-p bytecode)
        (error "Missing build-tool bytecode artifact: %s" bytecode)))))

(defun dream-build-clean-config-native ()
  "Delete native artifacts belonging to current owned configuration files."
  (dolist (source (dream-build-config-files))
    (let ((eln (comp-el-to-eln-filename source)))
      (when (and (file-exists-p eln)
                 (file-in-directory-p eln dream-eln-directory))
        (delete-file eln)))))

(defun dream-build--isolation-form (file destination)
  "Return the form a fresh Emacs evaluates to strictly compile FILE.
The subprocess reproduces the build session's compile context: the
parent's `load-path' plus the aggregate autoloads cache, so cookied
functions are known.  Bytecode goes below DESTINATION, leaving real
artifacts untouched."
  `(progn
     (setq user-emacs-directory ,user-emacs-directory)
     (setq load-path ',load-path)
     (when (file-readable-p ,dream-autoloads-file)
       (load ,(file-name-sans-extension dream-autoloads-file) nil t))
     (require 'bytecomp)
     (setq byte-compile-dest-file-function
           (lambda (source)
             (expand-file-name
              (concat (file-name-nondirectory source) "c")
              ,destination)))
     (let ((byte-compile-error-on-warn t)
           (byte-compile-warnings t))
       (unless (byte-compile-file ,file)
         (kill-emacs 1)))))

(defun dream-build--check-isolated-files (files)
  "Strictly byte-compile each of FILES in its own Emacs subprocess.
Return t when every file compiles cleanly; signal an error listing
each failure with its compiler output otherwise."
  (let ((destination (make-temp-file "dream-isolated-" t))
        (emacs (expand-file-name invocation-name invocation-directory))
        (print-length nil)
        (print-level nil)
        (failures nil))
    (unwind-protect
        (dolist (file files)
          (with-temp-buffer
            (unless (zerop (call-process
                            emacs nil t nil "-Q" "--batch" "--eval"
                            (prin1-to-string
                             (dream-build--isolation-form file destination))))
              (push (format "%s\n%s"
                            (file-relative-name file user-emacs-directory)
                            (buffer-string))
                    failures))))
      (delete-directory destination t))
    (when failures
      (error "Isolated compilation failed for %d file(s):\n%s"
             (length failures) (string-join (nreverse failures) "\n")))
    t))

(defun dream-build-check-isolated ()
  "Prove every owned file declares its own compile-time context.
The shared build session accumulates features loaded by earlier
files' compile-time requires; compiling each file in a fresh process
removes that masking."
  (interactive)
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "build" user-emacs-directory))
  (dream-paths-add-load-paths)
  (dream-build--check-isolated-files
   (append (dream-build-config-files) (dream-build-tool-files))))

(defun dream-build-check-declare ()
  "Verify every `declare-function' claim under the owned directories.
Needs the drones on `load-path' (check-declare resolves FILE via
`locate-library'), so run it from a Borg-initialized session."
  (interactive)
  (require 'check-declare)
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "build" user-emacs-directory))
  (dream-paths-add-load-paths)
  (let ((default-directory user-emacs-directory)
        (failures nil))
    (dolist (name '("core" "lib" "lisp" "build"))
      (let ((directory (expand-file-name name user-emacs-directory)))
        (when (file-directory-p directory)
          (when-let* ((errors (check-declare-directory directory)))
            (push (cons name errors) failures)))))
    (when failures
      (error "False `declare-function' statements: %S" (nreverse failures)))
    t))

(defun dream-build-autoloads ()
  "Generate and byte-compile the aggregate Borg autoload cache."
  (require 'dream-autoloads-build)
  (let ((source (dream-autoloads-generate)))
    ;; The aggregate consists of registration forms; native code adds no value.
    (dream-build--compile-file source nil nil)
    source))

(cl-defun dream-build-config
    (&optional config-native (packages-native nil packages-native-supplied-p))
  "Compile all owned runtime configuration files.
When CONFIG-NATIVE is non-nil, also compile native code synchronously.
PACKAGES-NATIVE records the Borg package artifact policy in the manifest."
  (interactive "P")
  (unless packages-native-supplied-p
    (setq packages-native
          (plist-get (dream-startup-read-manifest) :packages-native)))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "build" user-emacs-directory))
  (dream-paths-add-load-paths)
  (unless config-native
    (dream-build-clean-config-native))
  (dolist (file (dream-build-config-files))
    (dream-build--compile-file file config-native t))
  (dolist (file (dream-build-tool-files))
    (dream-build--compile-file file nil t))
  (dream-build--verify-config-artifacts config-native)
  (dream-build-write-manifest config-native packages-native))

(defun dream-build-after-borg-rebuild (&optional native)
  "Finish a full Borg rebuild, preserving its NATIVE build mode."
  (dream-build-autoloads)
  (dream-build-config dream-build-default-config-native native))

(defun dream-build-refresh-autoloads ()
  "Refresh aggregate autoload bytecode after an individual drone build."
  (let* ((old-manifest (dream-startup-read-manifest))
         (config-native (plist-get old-manifest :config-native))
         (packages-native (plist-get old-manifest :packages-native)))
    (dream-build-autoloads)
    (dream-build-write-manifest config-native packages-native)))

(provide 'dream-compile)
;;; dream-compile.el ends here.
