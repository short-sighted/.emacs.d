;;; dream-startup.el --- Startup policy for Dream Emacs. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'cl-lib)

(cl-eval-when (compile)
  ;; comp-run.el defines `native-comp-jit-compilation' and ships in
  ;; every Emacs 31 build, native or not.
  (require 'comp-run)
  (require 'once-incrementally))

(defvar on-init-ui-hook nil)
(defvar dream-startup--incremental-loading-started nil)

(defconst dream-build-manifest-schema 1)
(defconst dream-startup-gc-cons-threshold (* 64 1024 1024))
(defconst dream-startup-gc-cons-percentage 0.1)

(defun dream-startup-disable-runtime-compilation ()
  "Disable implicit native compilation for the current session."
  (setq native-comp-jit-compilation nil))

(defun dream-startup-restore-gc ()
  "Restore the steady-state garbage collector settings."
  (setq gc-cons-threshold dream-startup-gc-cons-threshold
        gc-cons-percentage dream-startup-gc-cons-percentage))

(defun dream-startup-native-version ()
  "Return the native compilation ABI identifier for this Emacs."
  (if (boundp 'comp-native-version-dir)
      comp-native-version-dir
    "none"))

(defun dream-startup-manifest-compatible-p (manifest)
  "Return non-nil when MANIFEST is usable by the running Emacs."
  (and (equal (plist-get manifest :schema) dream-build-manifest-schema)
       (equal (plist-get manifest :emacs-version) emacs-version)
       (equal (plist-get manifest :native-version)
              (dream-startup-native-version))
       (equal (file-name-as-directory
               (expand-file-name (plist-get manifest :eln-directory)
                                 user-emacs-directory))
              dream-eln-directory)
       (equal (plist-get manifest :lsp-use-plists)
              (getenv "LSP_USE_PLISTS"))))

(defun dream-startup-read-manifest ()
  "Read the build manifest, returning nil when it is absent or malformed."
  (when (file-readable-p dream-build-manifest-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents dream-build-manifest-file)
          (read (current-buffer)))
      (error nil))))

(defun dream-startup--file-hash (file)
  "Return the SHA-256 digest of FILE contents."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun dream-startup-manifest-sources-current-p (manifest)
  "Return non-nil when all owned sources recorded in MANIFEST are unchanged."
  (cl-every
   (lambda (entry)
     (let ((file (expand-file-name (car entry) user-emacs-directory)))
       (and (file-readable-p file)
            (equal (cadr entry) (dream-startup--file-hash file)))))
   (plist-get manifest :sources)))

(defun dream-startup-check-manifest ()
  "Warn when build artifacts were produced for a different runtime."
  (let ((manifest (dream-startup-read-manifest)))
    (cond
     ((null manifest)
      (display-warning 'dream-build
                       "Build manifest is missing; run `make native'."
                       :warning))
     ((not (dream-startup-manifest-compatible-p manifest))
      (display-warning
       'dream-build
       "Build manifest does not match this Emacs/native/LSP environment; using existing artifacts without recompiling."
       :warning))
     ((not (dream-startup-manifest-sources-current-p manifest))
      (display-warning
       'dream-build
       "Owned configuration sources changed after the last build; existing artifacts remain active until the next explicit build."
       :warning)))))

(defun dream-startup--start-incremental-loading ()
  "Start once.el incremental loading exactly once."
  (unless dream-startup--incremental-loading-started
    (setq dream-startup--incremental-loading-started t)
    (once-enable-incremental-loading)))

(defun dream-startup-arm-incremental-loading (&optional daemon)
  "Start incremental loading, waiting for UI when DAEMON is non-nil."
  (if daemon
      (add-hook 'on-init-ui-hook #'dream-startup--start-incremental-loading -100)
    (dream-startup--start-incremental-loading)))

(defun dream-startup--quiet-incremental-loading (function &rest args)
  "Call incremental loader FUNCTION with ARGS without echo-area noise."
  (let ((inhibit-message t))
    (apply function args)))

(defun dream-startup-initialize ()
  "Finalize Dream startup policy after setup.el and once.el are available."
  (dream-startup-disable-runtime-compilation)
  (setq once-idle-timer 1.5
        once-incremental-run-interval 1.5)
  (unless (advice-member-p #'dream-startup--quiet-incremental-loading
                           'once--run-incrementally)
    (advice-add 'once--run-incrementally
                :around #'dream-startup--quiet-incremental-loading))
  (if noninteractive
      (dream-startup-restore-gc)
    (add-hook 'emacs-startup-hook #'dream-startup-restore-gc 100))
  (run-with-idle-timer 3 nil #'dream-startup-check-manifest)
  (dream-startup-arm-incremental-loading (daemonp)))

(provide 'dream-startup)
;;; dream-startup.el ends here.
