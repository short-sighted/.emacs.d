;;; init-lang-cpp.el --- C and C++ language support. -*- lexical-binding: t; -*-

(require 'dream-setup)

(cl-eval-when (compile)
  (require 'c-ts-mode))

(setup c-ts-mode
  (:autoload c++-ts-mode)
  (:set c-ts-mode-enable-doxygen t
        c-ts-indent-offset 4))

(once (list :packages 'init-lsp)
  (add-hook 'c-ts-mode-hook #'lsp-deferred)
  (add-hook 'c++-ts-mode-hook #'lsp-deferred))

(provide 'init-lang-cpp)
;;; init-lang-cpp.el ends here.
