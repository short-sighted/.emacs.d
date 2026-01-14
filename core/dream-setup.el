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

;; setup
(setup-define :iload
  (lambda (&rest packages)
    `(dream-load-packages-incrementally '(,@packages)))
  :documentation "Load packages incrementally.")

(setup-define :option*
  (lambda (&rest body)
    `(cl-letf (((symbol-function 'message) #'format))
       ,(macroexp-progn body)))
  :documentation "Evaluate BODY but keep the echo era clean."
  :debug '(setup))

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

(setup borg
  (:option*
   borg-compile-function #'borg-byte+native-compile-async)
  (:when-loaded
    (advice-add 'borg-assimilate
		:after
		(lambda (package &rest _args)
		  (borg--call-git package "conifg" "-f"
				  borg-gitmodules-file
				  (format "submodule.%s.ignore" package)
				  "untracked")
		  (borg--call-git package "add" ".gitmodules")))))

(provide 'dream-setup)
;;; dream-setup.el ends here.
