;;; init-lang-web.el --- Web and Vue language support. -*- lexical-binding: t; -*-

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'lsp-mode)
  ;; `dream-lsp' carries no autoload cookie; see init-lsp.el.
  (require 'init-lsp))

(setup web-mode
  (:match-file "*.vue"))

(once (list :packages 'init-lsp)
  (add-hook 'web-mode-hook #'dream-lsp))

(once (list :packages 'init-lsp 'lsp-mode
            :check (lambda ()
                     (and (featurep 'init-lsp)
                          (featurep 'lsp-mode))))
  (add-to-list 'lsp-language-id-configuration '(web-mode . "vue")))

(provide 'init-lang-web)
;;; init-lang-web.el ends here.
