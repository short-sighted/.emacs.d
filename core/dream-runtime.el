;;; dream-runtime.el --- Deterministic runtime policy. -*- lexical-binding: t; -*-

(require 'dream-core)
(require 'dream-paths)

(cl-eval-when (compile)
  (require 'comp-common)
  (require 'comp-run)
  (require 'exec-path-from-shell))

(defconst dream-runtime-environment-schema 1)

(defconst dream-runtime-environment-variables
  '("PATH" "MANPATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
    "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH" "LSP_USE_PLISTS")
  "Environment variables captured by the explicit build workflow.")

(defconst dream-runtime-required-trampolines
  '(after-find-file display-startup-echo-area-message
    describe-buffer-bindings make-process rename-buffer all-completions
    rename-file)
  "Primitive trampolines prepared only by the explicit build workflow.")

(defconst dream-runtime--compiler-functions
  '(byte-compile-file native-compile native-compile-async
    comp-trampoline-compile)
  "Artifact-producing functions guarded during normal use.")

(defvar dream-runtime-compilation-allowed nil
  "Non-nil permits artifact compilation in the current dynamic scope.")

(defvar dream-runtime--guards-installed nil)

(defmacro dream-with-runtime-compilation (&rest body)
  "Evaluate BODY with explicit artifact compilation permission."
  (declare (indent 0) (debug t))
  `(let ((dream-runtime-compilation-allowed t))
     ,@body))

(defun dream-runtime--guard-compilation (function &rest args)
  "Call FUNCTION with ARGS only when compilation is explicitly allowed."
  (if dream-runtime-compilation-allowed
      (apply function args)
    (signal 'dream-runtime-compilation-error
            (list function))))

(defun dream-runtime--install-compilation-guards ()
  "Guard every automatic ELC, ELN, and trampoline compiler entry point."
  (dolist (function dream-runtime--compiler-functions)
    (unless (advice-member-p #'dream-runtime--guard-compilation function)
      (advice-add function :around #'dream-runtime--guard-compilation)))
  (setq dream-runtime--guards-installed t))

(defun dream-runtime--shell ()
  "Return the shell identity used by environment snapshots."
  (or (getenv "SHELL") shell-file-name ""))

(defun dream-runtime--collect-environment ()
  "Read the selected environment from an interactive login shell."
  (require 'exec-path-from-shell)
  (exec-path-from-shell-getenvs dream-runtime-environment-variables))

(defun dream-runtime-write-environment-snapshot ()
  "Atomically write the selected shell environment as inert Lisp data."
  (make-directory (file-name-directory dream-environment-file) t)
  (let ((temporary
         (make-temp-file
          (expand-file-name ".environment-"
                            (file-name-directory dream-environment-file))))
        (snapshot
         (list :schema dream-runtime-environment-schema
               :system-name (system-name)
               :system-type system-type
               :shell (dream-runtime--shell)
               :generated-at (current-time)
               :variables (dream-runtime--collect-environment))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil) (print-level nil))
              (prin1 snapshot (current-buffer))
              (insert "\n")))
          (set-file-modes temporary #o600)
          (rename-file temporary dream-environment-file t)
          snapshot)
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun dream-runtime-read-environment-snapshot ()
  "Read the environment snapshot as data, returning nil when malformed."
  (when (file-readable-p dream-environment-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents dream-environment-file)
          (read (current-buffer)))
      (error nil))))

(defun dream-runtime-environment-identity (&optional snapshot)
  "Return the machine identity portion of environment SNAPSHOT."
  (setq snapshot (or snapshot (dream-runtime-read-environment-snapshot)))
  (when snapshot
    (list :schema (plist-get snapshot :schema)
          :system-name (plist-get snapshot :system-name)
          :system-type (plist-get snapshot :system-type)
          :shell (plist-get snapshot :shell))))

(defun dream-runtime--environment-compatible-p (snapshot)
  "Return non-nil when SNAPSHOT belongs to this machine and shell."
  (let ((variables (plist-get snapshot :variables)))
    (and (equal (plist-get snapshot :schema)
                dream-runtime-environment-schema)
         (equal (plist-get snapshot :system-name) (system-name))
         (eq (plist-get snapshot :system-type) system-type)
         (equal (plist-get snapshot :shell) (dream-runtime--shell))
         (consp variables)
         (proper-list-p variables)
         (cl-every (lambda (pair)
                     (and (consp pair)
                          (stringp (car pair))
                          (or (null (cdr pair)) (stringp (cdr pair)))))
                   variables))))

(defun dream-runtime-apply-environment-snapshot (&optional snapshot)
  "Apply a compatible environment SNAPSHOT without executing shell code."
  (setq snapshot (or snapshot (dream-runtime-read-environment-snapshot)))
  (if (not (dream-runtime--environment-compatible-p snapshot))
      (progn
        (display-warning
         'dream-runtime
         "Shell environment snapshot is missing or incompatible; run `make runtime-artifacts'."
         :warning)
        nil)
    (dolist (pair (plist-get snapshot :variables))
      (setenv (car pair) (cdr pair)))
    (when-let* ((path (getenv "PATH")))
      (setq exec-path (append (parse-colon-path path) (list exec-directory)))
      (set-default 'eshell-path-env path))
    t))

(defun dream-environment-refresh ()
  "Explicitly refresh and apply the shell environment snapshot."
  (interactive)
  (dream-runtime-write-environment-snapshot)
  (dream-runtime-apply-environment-snapshot))

(defun dream-runtime-native-comp-available-p ()
  "Return non-nil when this Emacs can produce native Lisp artifacts."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)))

(defun dream-runtime-trampoline-contract ()
  "Return required trampolines for this Emacs build, or nil."
  (when (dream-runtime-native-comp-available-p)
    dream-runtime-required-trampolines))

(defun dream-runtime-trampoline-directory ()
  "Return the ABI-specific native trampoline directory, or nil."
  (when (dream-runtime-native-comp-available-p)
    (require 'comp-common)
    (file-name-as-directory
     (expand-file-name comp-native-version-dir dream-eln-directory))))

(defun dream-runtime-trampoline-file (primitive)
  "Return the expected prebuilt trampoline file for PRIMITIVE, or nil."
  (when-let* ((directory (dream-runtime-trampoline-directory)))
    (require 'comp-common)
    (expand-file-name (comp-trampoline-filename primitive) directory)))

(defun dream-runtime-trampolines-current-p ()
  "Return non-nil when every required trampoline exists for this ABI."
  (cl-every (lambda (primitive)
              (file-readable-p (dream-runtime-trampoline-file primitive)))
            (dream-runtime-trampoline-contract)))

(defun dream-runtime--disable-automatic-compilation ()
  "Disable automatic native compilation while retaining prebuilt ELN use."
  (setq native-comp-jit-compilation nil
        native-comp-enable-subr-trampolines
        (dream-runtime-trampoline-directory)))

(defun dream-runtime-initialize ()
  "Install deterministic runtime compilation and environment policy."
  (dream-runtime--disable-automatic-compilation)
  (dream-runtime--install-compilation-guards)
  (when (and (eq system-type 'darwin)
             (or (daemonp) (display-graphic-p)))
    (dream-runtime-apply-environment-snapshot)))

(provide 'dream-runtime)
;;; dream-runtime.el ends here.
