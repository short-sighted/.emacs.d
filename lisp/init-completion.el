;; init-completion.el --- Initialize completion configurations.     -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(cl-eval-when (compile)
  (require 'consult-imenu))

(setup vetico
  (:once (list :hooks 'pre-command-hook)
    (vertico-mode 1))
  (:opt* vertico-cycle t
	 vertico-count 17)
  (:after vertico
    (:hooks minibuffer-setup-hook vertico-repeat-save
	    rfn-eshadow-update-overlay-hook vertico-directory-tidy)
    (:after savehist
      (add-to-list 'savehist-additional-variables 'vertico-repeat-history))))

(setup orderless
  (:once (list :hooks 'pre-command-hook)
    (require 'orderless))
  (:opt completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (style orderless partial-completion)))
	orderless-component-separator #'orderless-escapable-split-on-space))

(setup consult
  (:after vertico
    (require 'consult))
  (:global
   "<remap> <switch-to-buffer>" consult-buffer
   "<remap> <switch-to-buffer-other-window>" consult-buffer-other-window
   "<remap> <switch-to-buffer-other-frame>" consult-buffer-other-frame
   "<remap> <goto-line>" consult-goto-line
   "<remap> <imenu>" consult-imenu
   "<remap> <yank-pop>" consult-yank-pop
   "C-s" consult-line))

(setup marginalia
  (:hook-into vertico-mode-hook))

(setup corfu
  (:once (list :hooks 'prog-mode-hook)
    (global-corfu-mode))
  (:opt corfu-cycle t
	corfu-auto t
	corfu-separator ?\s
	corfu-max-width 150
	corfu-auto-prefix 3
	corfu-auto-delay 0.2
	corfu-on-exact-match 'quit
	corfu-preselect nil
	corfu-margin-formatters '(nerd-icons-corfu-formatter)))

;;
;;; cape
(setup cape
  (dream/add-hook 'prog-mode-hook
    (defun +corfu-add-cape-file-h ()
      (add-hook 'completion-at-point-functions #'cape-file -10 t)))

  ;; Make these capfs composable.
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(setup yasnippet-capf
  (dream/add-hook 'yas-minor-mode-hook
    (defun +corfu-add-yasnippet-capf-h ()
      (add-hook 'completion-at-point-functions #'yasnippet-capf 30 t))))

;;
;;; Extensions
(setup corfu-history
  (:hook-into corfu-mode-hook)
  (:after savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(setup corfu-popupinfo
  (:hook-into corfu-mode-hook)
  (:opt corfu-popupinfo-delay '(0.5 . 1.0)))

(provide 'init-completion)
;;; init-completion.el ends here.
