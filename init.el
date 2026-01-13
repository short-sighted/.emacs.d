;; init.el --- Initialize Dream Emacs Configuration. -*- lexical-binding: t; -*-

(cl-eval-when (compile)
  (require 'once-setup))

;; Load Path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))

(declare-function vertico-mode "vertico")
(declare-function marginalia-mode "marginalia")

;; Core
(let ((file-name-handler-alist nil))
  (require 'dream-setup)

  (setup magit
  (:when-loaded
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-modules
			    'magit-insert-stashes
			    'append)))

(setup vetico
  (:once (list :hooks 'pre-command-hook)
    (vertico-mode 1)))

(setup marginalia
  (:hook-into vertico-mode-hook)))

(setq corfu-auto t
      corfu-delay t)
(global-corfu-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; init.el ends here.
