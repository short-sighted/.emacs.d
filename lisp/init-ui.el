;;; init-ui.el --- Frame and display configuration. -*- lexical-binding: t; -*-

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'doom-modeline))

;; Rule 5: projectile is not installed; doom-modeline merely honors
;; this variable when it is.
(defvar projectile-dynamic-mode-line)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      frame-title-format '("%b — Emacs")
      doom-modeline-bar-width 3
      doom-modeline-github nil
      doom-modeline-mu4e nil
      doom-modeline-persp-name nil
      doom-modeline-minor-modes nil
      doom-modeline-major-mode-icon nil
      doom-modeline-check 'simple
      doom-modeline-buffer-file-name-style 'relative-from-project
      doom-modeline-buffer-encoding 'nondefault
      doom-modeline-default-eol-type (if (eq system-type 'windows-nt) 1 0)
      projectile-dynamic-mode-line nil)

(setq-default display-line-numbers-width 3
              display-line-numbers-widen t)

(defun dream-ui--initialize ()
  "Enable UI-wide modes after the first usable frame exists."
  (doom-modeline-mode 1)
  (size-indication-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (pixel-scroll-precision-mode 1))

(add-hook 'dream-init-ui-hook #'dream-ui--initialize)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(provide 'init-ui)
;;; init-ui.el ends here.
