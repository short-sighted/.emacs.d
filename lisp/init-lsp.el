;;; init-lsp.el --- Language server configuration. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'dream-setup)

(cl-eval-when (compile)
  (require 'lsp-mode)
  ;; These two defcustoms live in their own files, not lsp-mode.el.
  (require 'lsp-diagnostics)
  (require 'lsp-completion)
  (require 'consult-lsp)
  (require 'eglot)
  (require 'consult-eglot))

(defcustom dream-lsp-client 'lsp-mode
  "Which LSP client `dream-lsp' starts in a buffer."
  :type '(choice (const :tag "lsp-mode" lsp-mode)
                 (const :tag "Eglot" eglot))
  :group 'dream)

(defun dream-lsp--client-function ()
  "Return the entry-point function for `dream-lsp-client'."
  (pcase dream-lsp-client
    ('lsp-mode #'lsp-deferred)
    ('eglot #'eglot-ensure)
    (other (user-error "Unknown dream-lsp-client: %S" other))))

(defun dream-lsp ()
  "Start the configured LSP client in the current buffer."
  (interactive)
  (funcall (dream-lsp--client-function)))

(setq lsp-session-file dream-lsp-session-file
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
  (:iload 0 dash f ht lv markdown-mode spinner s lsp-protocol)
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

(setup eglot
  (:set eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full)
        eglot-extend-to-xref t)
  (:when-loaded
    (keymap-set eglot-mode-map
                "<remap> <xref-find-apropos>" #'consult-eglot-symbols)
    ;; lsp-mode downloads the Vue server itself; Eglot needs the
    ;; command on PATH.  Adjust when the volar CLI changes.
    (add-to-list 'eglot-server-programs
                 '(web-mode . ("vue-language-server" "--stdio")))))

(provide 'init-lsp)
;;; init-lsp.el ends here.
