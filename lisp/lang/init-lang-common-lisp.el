;; init-lang-common-lisp -- Common Lisp Configuration.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(cl-eval-when (compile)
  (require 'sly)
  (require 'sly-autoloads))


(setup sly
  (:set sly-contribs '(sly-fancy)
        inferior-lisp-program "ros run -L sbcl-bin"
        sly-lisp-implementations
        '((sbcl ("ros" "-L" "sbcl-bin" "run"))
          (allegro ("ros" "-L" "allegro" "run"))))
  (:when-loaded (sly-setup)))

(provide 'init-lang-common-lisp)
;;; init-lang-common-lisp.el ends here.
