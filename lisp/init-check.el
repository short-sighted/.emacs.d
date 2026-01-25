;; init-check.el --- Initailize check configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;
;;; Flymake
(setup flymake
  (:hook-into prog-mode-hook)
  (:global "C-c f" #'flymake-show-buffer-diagnostics)
  (:bind-map flymake-diagnostics-buffer-mode-map
    "M-n" #'dream/flymake-diag-next-error
    "M-p" #'dream/flymake-diag-prev-error)
  (:opt flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe
        flymake-margin-indicator-position 'right-margin)
  (:when-loaded
    (defun dream-elisp-flymake-byte-compile (fn &rest args)
      "Wrapper for `elisp-flymake-byte-compile'."
      (let ((elisp-flymake-byte-compile-load-path
             (append elisp-flymake-byte-compile-load-path load-path)))
        (apply fn args)))
    (advice-add 'elisp-flymake-byte-compile :around #'dream-elisp-flymake-byte-compile)))

;;
;;; Flymake diagnostics navigation (errors only)
(defun dream/flymake--diag-error-line-p ()
  "Return non-nil when current diagnostics line is an error."
  (let* ((id (tabulated-list-get-id))
         (severity (plist-get id :severity)))
    (and severity (>= severity (warning-numeric-level :error)))))

(defun dream/flymake--diag-find-error (step &optional include-current)
  "Find an error line by stepping STEP; optionally INCLUDE-CURRENT."
  (let ((moved t)
        (found nil))
    (when include-current
      (setq found (dream/flymake--diag-error-line-p)))
    (while (and moved (not found))
      (setq moved (= (forward-line step) 0))
      (when moved
        (setq found (dream/flymake--diag-error-line-p))))
    found))

(defun dream/flymake--diag-step-to-error (step err-msg)
  "Move STEP lines until an error diagnostics line is found.
If no error is found before buffer boundary, wrap around once."
  (let ((start (point)))
    (unless (dream/flymake--diag-find-error step)
      (goto-char (if (> step 0) (point-min) (point-max)))
      (forward-line 0)
      (unless (dream/flymake--diag-find-error step t)
        (goto-char start)
        (user-error err-msg)))))

(defun dream/flymake-diag-next-error ()
  "Move to next error line in Flymake diagnostics buffer."
  (interactive)
  (unless (eq major-mode 'flymake-diagnostics-buffer-mode)
    (user-error "Not in Flymake diagnostics buffer"))
  (dream/flymake--diag-step-to-error 1 "No next error diagnostic"))

(defun dream/flymake-diag-prev-error ()
  "Move to previous error line in Flymake diagnostics buffer."
  (interactive)
  (unless (eq major-mode 'flymake-diagnostics-buffer-mode)
    (user-error "Not in Flymake diagnostics buffer"))
  (dream/flymake--diag-step-to-error -1 "No previous error diagnostic"))

(setup flymake-clippy
  (:require flymake-clippy)
  (:hooks rust-ts-mode-hook flymake-clippy-setup-backend))

;; (setup flyover
;;   (:hook-into flymake-mode-hook)
;;   (:opt flyover-checkers '(flymake)
;;         flyover-levels '(error)
;;         flyover-border-style 'arrow
;;         flyover-display-mode 'hide-on-same-line
;;         flyover-show-at-eol t
;;         flyover-virtual-line-type 'none))

(provide 'init-check)
;;; init-check.el ends here.
