;;; init-shell.el --- Initialize shell configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; shell configurations.
;;
;;; Code:

(cl-eval-when (compile)
  (require 'ghostel))

(setup ghostel
  (:autoload ghostel-send-key)
  (defun dream/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl")))

(provide 'init-shell)
;;; init-shell.el ends here.
