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
                      (window-height . 0.33)))))
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

;;
;;; Snippet
(setup yasnippet
  (:iload eldoc easymenu help-mode)
  (:hooks after-init-hook yas-global-mode)
  (:autoload yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-activate-extra-mode
             yas-deactivate-extra-mode
             yas-maybe-expand-abbrev-key-filter))

(setup js-jsx
  (:match-file "*.jsx")
  (:hooks js-jsx-mode-hook eglot-ensure)
  (:after eglot
    (add-to-list 'eglot-server-programs
                 '(js-jsx-mode . ("typescript-language-server" "--stdio")))))

(provide 'init-prog)
;;; init-prog.el ends here.
