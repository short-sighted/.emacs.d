;; dream-setup.el --- Initialize dream-setup. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; setup.el configuration.
;;
;;; Code:

(cl-eval-when (compile)
  (require 'borg)
  (require 'info)
  (require 'epkg))

(require 'setup)
(require 'once-setup)

(setup-define :after
  (lambda (feature &rest body)
    `(:with-feature ,feature
       (:when-loaded ,@body)))
  :documentation "EVAL BODY after FEATURE."
  :indent 1)

(setup-define :hooks
  (lambda (hook func)
    `(add-hook ',hook #',func))
  :documentation "Add pairs of hooks"
  :repeatable t)

(setup-define :init
  (lambda (&rest body)
    (macroexp-progn body))
  :documentation "Init keywords like `use-package' and `leaf'")

(provide 'dream-setup)
;;; dream-setup.el ends here.
