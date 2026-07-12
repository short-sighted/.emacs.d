;;; init-vc.el --- Version control configuration. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'magit)
  (require 'magit-status))

(setup magit
  (:set magit-auto-revert-mode nil)
  (:when-loaded
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-modules
                            'magit-insert-stashes
                            'append)))

(provide 'init-vc)
;;; init-vc.el ends here.
