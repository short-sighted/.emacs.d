;; init.el --- Initialize Dream Emacs Configuration. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-and-compile
  (defconst dream-local-directory (expand-file-name ".local" user-emacs-directory))
  (defconst dream-etc-directory (expand-file-name "etc" dream-local-directory))
  (defconst dream-var-directory (expand-file-name "var" dream-local-directory))
  (add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'dream-autoloads)
(dream-autoloads-initialize)

(let ((file-name-handler-alist nil))
  ;; Core
  (require 'dream-lib)
  (require 'dream-incremental-loading)
  (require 'dream-setup)
  (require 'dream-ui)
  (require 'dream-better-default)
  (require 'dream-editor)

  ;; Modules
  (require 'init-vc)
  (require 'init-completion)
  (require 'init-prog)
  (require 'init-lsp)
  (require 'init-check)
  (require 'init-utils)
  (require 'init-llm))

(provide 'init)
;;; init.el ends here.
