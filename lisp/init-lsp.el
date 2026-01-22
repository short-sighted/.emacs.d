;;; init-lsp.el --- Initialize language server configurations.  -*- lexical-binding: t; -*-
;;
;;; Commentray:
;;
;;; Code:

(setup eglot
  (:autoload eglot eglot-ensure)
  (:opt* read-process-output-max (* 1024 1024))
  (:opt eglot-sync-connect nil
        eglot-autoshutdown 1
        eglot-code-action-indications '(eldoc-hint))
  (:hooks eglot-managed-mode-hook eldoc-box-hover-at-point-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here.
