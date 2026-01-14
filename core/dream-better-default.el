;; dream-better-default.el --- Initialize dream better default configuration. -*- lexical-binding: t; -*-


;; Disable backup file mechanism
(setq make-backup-files nil)

(setup autorevert
  (:once (list :hooks 'find-file-hook)
    (:option global-auto-revert-non-file-buffers t)
    (global-auto-revert-mode 1)))

(setup delsel
  (:once (list :hooks 'pre-command-hook)
    (delete-selection-mode 1)))

;; pixel-scroll
(setup pixel-scroll
  (:once (list :hooks 'find-file-hook)
    (pixel-scroll-precision-mode))
  (setq scroll-preserve-screen-position 'always))

;; hideshow
(setup hideshow
  (:with-feature hs-minor-mode
    (:hook-into prog-mode-hook)))

(provide 'dream-better-default)
;;; dream-better-default.el ends here.
