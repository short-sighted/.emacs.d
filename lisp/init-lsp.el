;;; init-lsp.el --- Language server configuration. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'dream-setup)

(cl-eval-when (compile)
  (require 'lsp-mode)
  ;; These two defcustoms live in their own files, not lsp-mode.el.
  (require 'lsp-diagnostics)
  (require 'lsp-completion)
  (require 'consult-lsp))

(setq read-process-output-max (* 1024 1024)
      lsp-session-file dream-lsp-session-file
      lsp-server-install-dir dream-lsp-server-directory
      lsp-keep-workspace-alive nil
      lsp-log-io nil
      lsp-diagnostics-provider :flymake
      lsp-completion-provider :none
      lsp-enable-folding nil
      lsp-enable-text-document-color nil
      lsp-enable-on-type-formatting nil
      lsp-headerline-breadcrumb-enable nil)

(setup lsp-mode
  (:iload 0 dash f ht lv markdown-mode spinner s lsp-protocol 20 lsp-mode)
  (:autoload lsp-install-server)
  (:hook lsp-completion-mode))

(once (list :packages 'lsp-protocol)
  (lambda ()
    (unless (equal (and lsp-use-plists t)
                   (and (getenv "LSP_USE_PLISTS") t))
      (display-warning
       'dream-lsp
       "lsp-mode was compiled with a different LSP_USE_PLISTS setting; run `make native'."
       :warning))))

(setup consult-lsp
  (:require-once
      (list :packages 'init-completion 'consult 'lsp-mode
            :check (lambda ()
                     (and (featurep 'init-completion)
                          (featurep 'consult)
                          (featurep 'lsp-mode))))
    'consult-lsp)
  (:when-loaded
    (keymap-set lsp-mode-map
                "<remap> <xref-find-apropos>" #'consult-lsp-symbols)))

(provide 'init-lsp)
;;; init-lsp.el ends here.
