;;; init-lsp.el --- Initialize language server configurations.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(setup lsp-mode
  (:autoload lsp-install-server)
  (:when-loaded (fset #'jsonrpc--log-event #'ignore)
                (setq read-process-output-max (* 1024 1024))
                (setq lsp-log-io nil))
  (:init
   (setq lsp-session-file (expand-file-name "lsp-session" dream-var-directory)
         lsp-server-install-dir (file-name-concat dream-local-directory "lsp/"))
   (setq lsp-keep-workspace nil)

   (setq lsp-enable-folding nil
         lsp-enable-text-document-color nil
         lsp-enable-on-type-formatting nil
         lsp-headerline-breadcrumb-enable nil)

   (setq lsp-completion-provider :none)
   (add-hook 'lsp-mode-hook #'lsp-completion-mode)))


(setup consult-lsp
  (:if-feature consult)
  (:init
   (:with-map lsp-mode-map
    (:bind [remap xref-find-apropos] #'consult-lsp-symbols))))

(provide 'init-lsp)
;;; init-lsp.el ends here.
