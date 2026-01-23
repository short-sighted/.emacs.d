;; init-prog.el ---Initialize Programming Configurations.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:


;;
;;; Eldoc
(setup eldoc
  (:opt eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-prefer-doc-buffer 'maybe)
  (:when-loaded
    (defun dream/eldoc-doc-buffer-bottom (&optional interactive)
    "Get or display ElDoc documentation buffer in a bottom side window."
    (interactive (list t))
    (unless (buffer-live-p eldoc--doc-buffer)
      (user-error (format
                   "ElDoc buffer doesn't exist, maybe `%s' to produce one."
                   (substitute-command-keys "\\[eldoc]"))))
    (with-current-buffer eldoc--doc-buffer
      (cond
       (interactive
        (rename-buffer (replace-regexp-in-string "^ *" "" (buffer-name)) t)
        (let ((win (display-buffer-in-side-window
                    (current-buffer)
                    '((side . bottom)
                      (slot . 0)
                      (window-height . 0.2)))))
          (select-window win)
          (local-set-key (kbd "C-g") #'quit-window)))
       (t (current-buffer)))))
    (advice-add 'eldoc-doc-buffer :override #'dream/eldoc-doc-buffer-bottom)))

;;
;;; Treesit

(setup treesit
  (:if-feature treesit)
  (:only-if (treesit-available-p))
  (:opt treesit-enabled-modes t
        treesit-font-lock-level 4))

(provide 'init-prog)
;;; init-prog.el ends here.
