;; init.el --- Initialize Dream Emacs Configuration. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; load-path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(defconst dream-local-directory (expand-file-name ".local" user-emacs-directory))
(defconst dream-etc-directory (expand-file-name "etc" dream-local-directory))
(defconst dream-var-directory (expand-file-name "var" dream-local-directory))


(let ((file-name-handler-alist nil))
  ;; Core
  (require 'dream-incremental-loading)
  (require 'dream-setup)
  (require 'dream-ui)
  (require 'dream-better-default)
  (require 'dream-editor)

  ;; Modules
  (require 'init-vc)
  (require 'init-completion)
  (require 'init-lsp))

(provide 'init)
;;; init.el ends here.
