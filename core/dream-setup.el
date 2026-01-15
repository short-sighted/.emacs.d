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

(setup-define :global
  (lambda (key command)
    `(keymap-global-set ,key ,command))
  :documentation "Bind KEY to COMMAND in the global map.
KEY must be a string accepted by `keymap-global-set'.
For remapping, use \"<remap> <old-command>\" format.

Examples:
  (:global \"C-c C-c\" my-command)
  (:global \"<remap> <kill-line>\" my-kill-line)"
  :debug '(form sexp)
  :ensure '(nil func)
  :repeatable t)

(setup-define :opt
  (lambda (name val) `(customize-set-variable ',name ,val))
  :documentation "Customize variables."
  :after-loaded t
  :repeatable t)

(setup-define :opt*
  (lambda (name val) `(customize-set-variable ',name ,val))
  :documentation "Customize variables."
  :debug '(sexp form)
  :repeatable t)

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

(setup epkg
  (:iload epkg)
  (:opt* epkg-repository (expand-file-name "epkg/" dream-var-directory)))

(setup borg
  (:option*
   borg-compile-function #'borg-byte+native-compile-async)
  (:when-loaded
    (advice-add 'borg-assimilate
		:after
		(lambda (package &rest _args)
		  (borg--call-git package "config" "-f"
				  borg-gitmodules-file
				  (format "submodule.%s.ignore" package)
				  "untracked")
		  (borg--call-git package "add" ".gitmodules")))))

(provide 'dream-setup)
;;; dream-setup.el ends here.
