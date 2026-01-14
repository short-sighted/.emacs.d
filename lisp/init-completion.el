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
  (:global [remap goto-line] #'consult-goto-line))

(setup marginalia
  (:hook-into vertico-mode-hook))

(provide 'init-completion)
;;; init-completion.el ends here.
