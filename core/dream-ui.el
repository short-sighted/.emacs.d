;; dream-ui.el  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; modeline
(setup mood-line
  (:hook-into after-init-hook)
  (:opt
   mood-line-glyph-alist mood-line-glyphs-fira-code))

(provide 'dream-ui)
;;; dream-ui.el ends here.
