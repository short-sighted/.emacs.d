;;; init-lang.el --- Language collection entry. -*- lexical-binding: t; -*-

;;; Commentary:
;; Comment out a `require' below to disable one language member at runtime.

;;; Code:

(require 'init-lang-programming)
(require 'init-lang-common-lisp)
(require 'init-lang-cpp)
(require 'init-lang-rust)
(require 'init-lang-web)

(provide 'init-lang)
;;; init-lang.el ends here.
