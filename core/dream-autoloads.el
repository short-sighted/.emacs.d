;;; dream-autoloads.el --- Load the generated Borg autoload cache. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'cl-lib)

(cl-eval-when (compile)
  (require 'borg))

(defcustom dream-autoloads-stale-check-delay 2
  "Idle seconds before checking the generated autoload cache."
  :type '(choice (const :tag "Disabled" nil) number)
  :group 'dream)

(defconst dream-autoloads-state-version 6)

(defun dream-autoloads--warn (format-string &rest args)
  "Display a Dream autoload warning using FORMAT-STRING and ARGS."
  (display-warning 'dream-autoloads (apply #'format format-string args) :warning))

(defun dream-autoloads--read-state ()
  "Read the generated autoload state, returning nil on malformed data."
  (when (file-readable-p dream-autoloads-state-file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents dream-autoloads-state-file)
          (read (current-buffer)))
      (error nil))))

(defun dream-autoloads--state-entry-current-p (entry &optional directory)
  "Return non-nil when state ENTRY still describes its file or DIRECTORY."
  (let* ((file (expand-file-name (car entry) user-emacs-directory))
         (attributes (file-attributes file)))
    (and attributes
         (equal (nth 1 entry) (file-attribute-modification-time attributes))
         (or directory (equal (nth 2 entry) (file-attribute-size attributes))))))

(defun dream-autoloads-state-current-p ()
  "Return non-nil when generated autoload inputs match their saved state."
  (when-let* ((state (dream-autoloads--read-state)))
    (and (equal (plist-get state :version) dream-autoloads-state-version)
         (cl-every #'dream-autoloads--state-entry-current-p
                   (plist-get state :files))
         (cl-every (lambda (entry)
                     (dream-autoloads--state-entry-current-p entry t))
                   (plist-get state :directories)))))

(defun dream-autoloads-check-state ()
  "Warn when the generated autoload cache is stale."
  (unless (dream-autoloads-state-current-p)
    (dream-autoloads--warn
     "Autoload cache is stale; continuing with existing artifacts. Run `make build'.")))

(defun dream-autoloads--schedule-state-check ()
  "Schedule the read-only autoload state check."
  (when (numberp dream-autoloads-stale-check-delay)
    (if (zerop dream-autoloads-stale-check-delay)
        (dream-autoloads-check-state)
      (run-with-idle-timer dream-autoloads-stale-check-delay
                           nil #'dream-autoloads-check-state))))

(defun dream-autoloads--fallback-to-borg (reason)
  "Initialize Borg after reporting autoload cache failure REASON."
  (dream-autoloads--warn "%s Falling back to Borg initialization." reason)
  (add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
  (require 'borg)
  (borg-initialize))

(defun dream-autoloads--load-generated-cache ()
  "Load compiled generated autoloads, falling back to source."
  (let ((bytecode (concat (file-name-sans-extension dream-autoloads-file)
                          ".elc")))
    (cond
     ((file-readable-p bytecode)
      (condition-case err
          (load bytecode nil t t)
        (error
         (dream-autoloads--warn "Compiled autoload cache failed: %S" err)
         (when (file-readable-p dream-autoloads-file)
           (let ((native-comp-jit-compilation nil))
             (load dream-autoloads-file nil t t))))))
     ((file-readable-p dream-autoloads-file)
      (dream-autoloads--warn "Compiled autoload cache is missing; loading source.")
      (let ((native-comp-jit-compilation nil))
        (load dream-autoloads-file nil t t)))
     (t nil))))

(defun dream-autoloads-initialize ()
  "Load generated Borg autoloads without compiling at runtime."
  (if (dream-autoloads--load-generated-cache)
      (progn (dream-autoloads--schedule-state-check) t)
    (dream-autoloads--fallback-to-borg "Autoload cache is missing.")))

(provide 'dream-autoloads)
;;; dream-autoloads.el ends here.
