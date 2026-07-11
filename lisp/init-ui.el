;;; init-ui.el --- Frame and display configuration. -*- lexical-binding: t; -*-

(require 'dream-lib)
(require 'dream-setup)
(require 'seq)

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

(defvar dream-ui--fonts nil)

(defun dream-ui--first-font (families)
  "Return the first available font from FAMILIES."
  (seq-find #'dream-font-available-p families))

(defun dream-ui--select-fonts ()
  "Cache the preferred available fonts."
  (or dream-ui--fonts
      (setq dream-ui--fonts
            (list
             :default (dream-ui--first-font
                       '("Cascadia Code" "JetBrains Mono" "SF Mono" "Menlo"
                         "Hack" "Source Code Pro" "Monaco" "DejaVu Sans Mono"
                         "Consolas"))
             :symbol (dream-ui--first-font
                      '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol"))
             :emoji (dream-ui--first-font
                     '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji"))
             :han (dream-ui--first-font
                   '("LXGW Neo Xihei" "LXGW WenKai Mono"
                     "WenQuanYi Micro Hei Mono" "PingFang SC"
                     "Microsoft Yahei UI" "Simhei"))))))

(defun dream-ui-apply-fonts (&optional frame)
  "Apply cached font choices to FRAME."
  (let ((frame (or frame (selected-frame))))
    (unless (or (frame-initial-p frame) (not (display-graphic-p frame)))
      (with-selected-frame frame
        (let ((fonts (dream-ui--select-fonts)))
          (when-let* ((font (plist-get fonts :default)))
            (set-face-attribute 'default frame :family font :height 110))
          (when-let* ((font (plist-get fonts :symbol)))
            (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
          (when-let* ((font (plist-get fonts :emoji)))
            (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))
          (when-let* ((font (plist-get fonts :han)))
            (setf (alist-get font face-font-rescale-alist nil nil #'equal) 1.3)
            (set-fontset-font t 'han (font-spec :family font)))
          (when (dream-font-available-p "Symbols Nerd Font Mono")
            (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
              (set-fontset-font t range "Symbols Nerd Font Mono"))))))))

(defun dream-ui-initialize ()
  "Enable UI-wide modes after the first usable frame exists."
  (doom-modeline-mode 1)
  (size-indication-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (pixel-scroll-precision-mode 1)
  (dream-ui-apply-fonts))

(add-hook 'on-init-ui-hook #'dream-ui-initialize)
(add-hook 'server-after-make-frame-hook #'dream-ui-apply-fonts)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(provide 'init-ui)
;;; init-ui.el ends here.
