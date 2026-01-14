;; init.el --- Initialize Dream Emacs Configuration. -*- lexical-binding: t; -*-

;; load-path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(eval-when-compile
  (require 'once-setup))

(defconst dream-local-directory (expand-file-name ".local" user-emacs-directory))
(defconst dream-etc-directory (expand-file-name "etc" dream-local-directory))
(defconst dream-var-directory (expand-file-name "var" dream-local-directory))

(declare-function vertico-mode "vertico")
(declare-function marginalia-mode "marginalia")

;; Core
(let ((file-name-handler-alist nil))
  (require 'dream-incremental-loading)
  (require 'dream-setup)
  (require 'dream-better-default))


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
  (:hook-into vertico-mode-hook))

(setq corfu-auto t
      corfu-delay t)
(global-corfu-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; init.el ends here.
