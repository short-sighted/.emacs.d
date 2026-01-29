;;; dream-better-default.el --- Initialize dream better default configuration. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(cl-eval-when (compile)
  (require 'dream-setup))

;; Disable backup file mechanism
(setq make-backup-files nil)

(setup autorevert
  (:once (list :hooks 'find-file-hook)
    (:option global-auto-revert-non-file-buffers t)
    (global-auto-revert-mode 1)))

(setup delsel
  (:once (list :hooks 'pre-command-hook)
    (delete-selection-mode 1)))

;; recentf
(setup recentf
  (:once (list :hooks 'pre-command-hook)
    (recentf-mode))
  (:when-loaded
    (:option recentf-auto-cleanup 'never
	     recentf-max-menu-items 50
	     recentf-max-saved-items 50
	     recentf-save-file (expand-file-name "recentf-save.el" dream-var-directory))))

;; pixel-scroll
(setup pixel-scroll
  (:once (list :hooks 'find-file-hook)
    (pixel-scroll-precision-mode))
  (setq scroll-preserve-screen-position 'always))

;; hideshow
(setup hideshow
  (:with-feature hs-minor-mode
    (:hook-into prog-mode-hook))
  (:with-map hs-minor-mode-map
    (:bind "s-<tab>" hs-cycle)))

(provide 'dream-better-default)
;;; dream-better-default.el ends here.
