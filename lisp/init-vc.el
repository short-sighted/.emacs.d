;; init-vc.el --- Initialize vc configurations. -*- lexical-binding: t; -*- 
;;
;;; Commentary:
;;
;;; Code:

;; magit
(setup magit
  (:iload magit)
  (:when-loaded
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-modules
			    'magit-insert-stashes
			    'append)))

(provide 'init-vc)
;;; init-vc.el ends here.
