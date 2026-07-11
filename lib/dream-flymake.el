;;; dream-flymake.el --- Error-row navigation for Flymake. -*- lexical-binding: t; -*-

(require 'flymake)

(defun dream-flymake--error-row-p ()
  "Return non-nil when the current Emacs 31 diagnostics row is an error.
The plist ID is created by `flymake--tabulated-setup-1' in Emacs 31."
  (when-let* ((id (tabulated-list-get-id))
              (severity (plist-get id :severity)))
    (>= severity (warning-numeric-level :error))))

(defun dream-flymake--find-error (step &optional include-current)
  "Move STEP rows to an error, optionally checking INCLUDE-CURRENT first."
  (let ((found (and include-current (dream-flymake--error-row-p)))
        (moved t))
    (while (and moved (not found))
      (setq moved (= (forward-line step) 0)
            found (and moved (dream-flymake--error-row-p))))
    found))

(defun dream-flymake--step-to-error (step message)
  "Move STEP rows to an error or report MESSAGE."
  (let ((start (point)))
    (unless (dream-flymake--find-error step)
      (goto-char (if (> step 0) (point-min) (point-max)))
      (forward-line 0)
      (unless (dream-flymake--find-error step t)
        (goto-char start)
        (user-error "%s" message)))))

(defun dream-flymake-next-error ()
  "Move to the next error row in a Flymake diagnostics buffer."
  (interactive)
  (dream-flymake--step-to-error 1 "No next error diagnostic"))

(defun dream-flymake-previous-error ()
  "Move to the previous error row in a Flymake diagnostics buffer."
  (interactive)
  (dream-flymake--step-to-error -1 "No previous error diagnostic"))

(keymap-set flymake-diagnostics-buffer-mode-map
            "M-n" #'dream-flymake-next-error)
(keymap-set flymake-diagnostics-buffer-mode-map
            "M-p" #'dream-flymake-previous-error)

(provide 'dream-flymake)
;;; dream-flymake.el ends here.
