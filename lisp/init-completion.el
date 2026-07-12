;;; init-completion.el --- Minibuffer and in-buffer completion. -*- lexical-binding: t; -*-

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'savehist)
  (require 'vertico)
  (require 'orderless)
  (require 'corfu)
  (require 'corfu-auto)
  (require 'corfu-popupinfo)
  (require 'cape))

(setup vertico
  (:iload -80 savehist orderless -70 vertico)
  (:once (list :hooks
               (list :hook 'dream-first-input-hook :depth -70))
    (vertico-mode 1))
  (:set vertico-cycle t
        vertico-count 17)
  (:when-loaded
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))

(setup orderless
  (:require-once (list :hooks 'dream-first-input-hook) 'orderless)
  (:set completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles orderless partial-completion)))
        completion-pcm-leading-wildcard t
        orderless-component-separator #'orderless-escapable-split))

(setup consult
  (:global
   "<remap> <switch-to-buffer>" consult-buffer
   "<remap> <switch-to-buffer-other-window>" consult-buffer-other-window
   "<remap> <switch-to-buffer-other-frame>" consult-buffer-other-frame
   "<remap> <goto-line>" consult-goto-line
   "<remap> <imenu>" consult-imenu
   "<remap> <yank-pop>" consult-yank-pop
   "C-s" consult-line))

(setup marginalia
  (:hook-into vertico-mode))

(setup corfu
  (:hooks dream-first-input-hook global-corfu-mode)
  (:set corfu-cycle t
        corfu-auto t
        corfu-count 16
        corfu-max-width 120
        corfu-auto-prefix 2
        corfu-auto-delay 0.24
        corfu-preselect 'prompt
        corfu-on-exact-match nil
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator
        corfu-margin-formatters '(nerd-icons-corfu-formatter))
  (:once (list :packages 'corfu)
    (require 'corfu-history)
    (require 'corfu-popupinfo)
    (setq corfu-popupinfo-delay '(0.5 . 1.0))
    (corfu-history-mode 1)
    (corfu-popupinfo-mode 1)
    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history))))

(defun dream-completion--add-file-capf ()
  "Add file completion to the current programming buffer."
  (add-hook 'completion-at-point-functions #'cape-file -10 t))

(add-hook 'prog-mode-hook #'dream-completion--add-file-capf)

(defun dream-completion--advise-capf (function &rest wrappers)
  "Add each function in WRAPPERS around completion FUNCTION once."
  (dolist (wrapper wrappers)
    (unless (advice-member-p wrapper function)
      (advice-add function :around wrapper))))

(once (list :packages 'lsp-completion)
  (lambda ()
    (dream-completion--advise-capf
     'lsp-completion-at-point
     #'cape-wrap-noninterruptible #'cape-wrap-nonexclusive)))
(once (list :packages 'comint)
  (lambda ()
    (dream-completion--advise-capf
     'comint-completion-at-point #'cape-wrap-nonexclusive)))
(once (list :packages 'eglot)
  (lambda ()
    (dream-completion--advise-capf
     'eglot-completion-at-point #'cape-wrap-nonexclusive)))
(once (list :packages 'pcomplete)
  (lambda ()
    (dream-completion--advise-capf
     'pcomplete-completions-at-point #'cape-wrap-nonexclusive)))

(provide 'init-completion)
;;; init-completion.el ends here.
