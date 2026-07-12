;;; init-lang-rust.el --- Rust language support. -*- lexical-binding: t; -*-

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'flymake-clippy)
  ;; `dream-lsp' carries no autoload cookie; see init-lsp.el.
  (require 'init-lsp))

(once (list :packages 'init-lsp)
  (add-hook 'rust-ts-mode-hook #'dream-lsp))

(setup flymake-clippy
  (:needs "cargo")
  (:autoload flymake-clippy-setup-backend)
  (:with-hook rust-ts-mode-hook
    (:hook flymake-clippy-setup-backend)))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here.
