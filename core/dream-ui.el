;; dream-ui.el  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'dream-lib)

;;
;;; Modeline
(setup mood-line
  (:hook-into after-init-hook)
  (:opt
   mood-line-glyph-alist mood-line-glyphs-fira-code))

;;
;;; Highlight the current line
(setup hl-line
  (:with-hook after-init-hook
    (:hook global-hl-line-mode)))

;;
;;; Font
(defun dream-init-font-h()
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Jetbrains Mono"
                           "SF Mono" "Menlo" "Hack" "Source Code Pro"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
	     when (dream/font-available-p font)
             return (set-face-attribute 'default nil
					:family font
                                        :height 110)))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
	   when (dream/font-available-p font)
	   return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))

  ;; Emoji
  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
	   when (dream/font-available-p font)
	   return (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("LXGW Neo Xihei" "LXGW WenKai Mono" "WenQuanYi Micro Hei Mono"
			 "PingFang SC" "Microsoft Yahei UI" "Simhei")
	   when (dream/font-available-p font)
	   return (progn
		    (add-to-list 'face-font-rescale-alist `(,font . 1.3))
		    (set-fontset-font t 'han (font-spec :family font))))

  ;; Nerd Fonts use these Private Use Areas.
  (cl-loop for range in '((#xe000 . #xf8ff) (#xf0000 . #xfffff))
	   return (set-fontset-font t range "Symbols Nerd Font Mono")))
(add-hook 'window-setup-hook #'dream-init-font-h)
(add-hook 'server-after-make-frame-hook #'dream-init-font-h)

;;
;;; Line numbers

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions to make it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(dream/add-hook '(prog-mode-hook text-mode-hook)
                 #'display-line-numbers-mode)

;;
;;; Misc

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(provide 'dream-ui)
;;; dream-ui.el ends here.
