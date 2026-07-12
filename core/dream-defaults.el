;;; dream-defaults.el --- Better baseline defaults. -*- lexical-binding: t; -*-

(require 'dream-hooks)

(cl-eval-when (compile)
  ;; `gnutls-verify-error' and friends are not preloaded.
  (require 'gnutls))

;; Rule 5: platform modifier variables live in each build's C code; the
;; other platform's compiler cannot require them from anywhere.
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-right-option-modifier)
(defvar ns-command-modifier)
(defvar ns-option-modifier)
(defvar ns-right-option-modifier)
(defvar w32-lwindow-modifier)
(defvar w32-rwindow-modifier)

;;; Encodings

(set-language-environment "UTF-8")
;; `set-language-environment' also sets `default-input-method'; undo that.
(setq default-input-method nil)
;; The Windows clipboard tends to be UTF-16; leave it alone there.
(unless (eq system-type 'windows-nt)
  (setq selection-coding-system 'utf-8))

;;; Security

(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (eq system-type 'windows-nt))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      gnutls-min-prime-bits 3072)

;;; Redisplay performance

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;;; Files

(setq find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t)

(defun dream-defaults--create-missing-directories ()
  "Offer to create the parent directories of a nonexistent visited file."
  (unless (file-remote-p buffer-file-name)
    (let ((parent (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent))
           (progn (make-directory parent 'parents) t)))))

(add-hook 'find-file-not-found-functions
          #'dream-defaults--create-missing-directories)

(defun dream-defaults--quiet-autosave-notice (function &rest args)
  "Call FUNCTION with ARGS without pausing on auto-save-data notices."
  (cl-letf (((symbol-function 'sit-for) #'ignore))
    (apply function args)))

(advice-add 'after-find-file :around #'dream-defaults--quiet-autosave-notice)

;;; Formatting

(setq-default fill-column 80
              word-wrap t
              truncate-lines t)
(setq truncate-partial-width-windows nil
      sentence-end-double-space nil
      require-final-newline t)
(add-hook 'text-mode-hook #'visual-line-mode)

;;; Frames and windows

(setq frame-resize-pixelwise t
      window-resize-pixelwise nil
      use-dialog-box nil
      split-width-threshold 160
      split-height-threshold nil
      window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'dream-init-ui-hook #'window-divider-mode)

;;; Minibuffer

(setq read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      use-short-answers t
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
;; SPC-as-yes is too easy to hit by accident.
(keymap-unset y-or-n-p-map "SPC")
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Cursor and bells

(blink-cursor-mode -1)
(setq blink-matching-paren nil
      x-stretch-cursor nil
      ring-bell-function #'ignore
      visible-bell nil)

;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;; Buffers and fringes

(setq uniquify-buffer-name-style 'forward
      confirm-nonexistent-file-or-buffer nil
      mouse-yank-at-point t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;; Matching parens (`show-paren-mode' is global by default since 28)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Platform modifiers

(cond
 ((eq system-type 'darwin)
  (setq mac-command-modifier 'super
        ns-command-modifier 'super
        mac-option-modifier 'meta
        ns-option-modifier 'meta
        mac-right-option-modifier 'none
        ns-right-option-modifier 'none))
 ((eq system-type 'windows-nt)
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;;; Universal escape

(defvar dream-escape-hook nil
  "Hook run by `dream-escape' before falling back to `keyboard-quit'.
When any function returns non-nil, the remaining functions and the
fallback are skipped.")

(defun dream-escape (&optional interactive)
  "Quit the minibuffer, run `dream-escape-hook', or `keyboard-quit'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ((run-hook-with-args-until-success 'dream-escape-hook))
          ;; Never interrupt macro recording or replay.
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(keymap-global-set "<remap> <keyboard-quit>" #'dream-escape)

(provide 'dream-defaults)
;;; dream-defaults.el ends here.
