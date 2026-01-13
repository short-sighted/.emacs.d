;; -*- lexical-binding: t; -*-

(require 'setup)
(setq corfu-auto t
      corfu-delay t)
(global-corfu-mode)
(add-hook 'prog-mode #'display-line-numbers-mode)

(setup magit
  (:when-loaded
    (magit-add-section-hook 'magit-status-sections-hook
			    'magit-insert-modules
			    'magit-insert-stashes
			    'append)))

;; init.el ends here.
