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

;; misc
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(provide 'dream-ui)
;;; dream-ui.el ends here.
