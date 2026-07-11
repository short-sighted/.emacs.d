;;; init.el --- Dream Emacs configuration entry point. -*- lexical-binding: t; -*-

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (require 'dream-paths)
  (dream-paths-add-load-paths))

(dream-paths-initialize)

(require 'dream-startup)
(require 'dream-autoloads)
(dream-autoloads-initialize)

(let ((file-name-handler-alist nil))
  ;; Core
  (require 'dream-lib)
  (require 'dream-setup)

  ;; Editor
  (require 'init-editing)
  (require 'init-ui)
  (require 'init-completion)

  ;; Programming
  (require 'init-lsp)
  (require 'init-lang)
  (require 'init-spell)

  ;; Tools
  (require 'init-vc)
  (require 'init-tools)
  (require 'init-llm))

(dream-startup-initialize)

(provide 'init)
;;; init.el ends here.
