;; -*- lexical-binding: t; -*-

(require 'setup)
(require 'once-setup)
(setq corfu-auto t
      corfu-delay t)
(global-corfu-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(setup magit
  (:when-loaded
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-modules
			    'magit-insert-stashes
			    'append)))

(declare-function vertico-mode "vertico")
(setup vetico
  (:once (list :hooks 'pre-command-hook)
    (vertico-mode 1)))

(declare-function marginalia-mode "marginalia")
(setup marginalia
  (:hook-into vertico-mode-hook))

;; init.el ends here.
