;; dream-lib.el --- Dream Emacs Library.  -*- lexical-binding: t; -*-

;; Font
(defun dream/font-available-p(font-name)
  "Check if not with FONT-NAME is avaiable."
  (find-font (font-spec :name font-name)))

(provide 'dream-lib)
;;; dream-lib.el ends here.
