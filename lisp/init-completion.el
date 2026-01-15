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
	 vertico-count 17))

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

;;cape
(setup cape
  (:once (list :before 'global-corfu-mode)
    (:hooks completion-at-point-functions cape-file)))

(provide 'init-completion)
;;; init-completion.el ends here.
