;;; init-project.el --- Project Manager Configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(cl-eval-when (compile)
  (require 'projectile))

(setup projectile
  (:autoload projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-locate-dominating-file
             projectile-relevant-known-projects)
  (:hook-into dream-first-buffer-hook)
  (:bind "C-c p" projectile-command-map)
  (setq projectile-enable-caching (if noninteractive t 'presistent)
        projectile-globally-ignored-files '(".DS_Store" "TAGS")
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".eln")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-ignored-projects '("~/")
        projectile-known-projects-file (file-name-concat dream-cache-directory "project.eld")))

(provide 'init-project)
;;; init-project.el ends here.
