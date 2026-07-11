;;; dream-setup.el --- setup.el integration for Dream Emacs. -*- lexical-binding: t; -*-

(require 'setup)

(eval-and-compile
  (defvar once-setup-keyword-aliases)
  ;; once-setup reads this exactly once while it is loaded.
  (setq once-setup-keyword-aliases
        '(:once-x-require :require-once
          :once-require-incrementally :iload)))
(require 'once-setup)
(require 'on)

(setup-define :global
  (lambda (key command) `(keymap-global-set ,key ,command))
  :documentation "Bind KEY to COMMAND in the global map."
  :debug '(form sexp)
  :ensure '(nil func)
  :repeatable t)

(setup-define :autoload
  (lambda (function)
    (let ((name (if (memq (car-safe function) '(quote function))
                    (cadr function)
                  function)))
      `(unless (fboundp ',name)
         (autoload #',name ,(symbol-name (setup-get 'feature)) nil t))))
  :documentation "Autoload FUNCTION from the current feature."
  :repeatable t
  :signature '(FUNCTION ...))

(setup-define :set
  (lambda (name value) `(setq ,name ,value))
  :documentation "Set NAME to VALUE during configuration registration."
  :debug '(sexp form)
  :repeatable t)

(setup-define :advice
  (lambda (symbol where function) `(advice-add ',symbol ,where ,function))
  :documentation "Advise SYMBOL after the current feature loads."
  :after-loaded t
  :debug '(sexp sexp function-form)
  :ensure '(nil nil func)
  :repeatable t)

(setup-define :needs
  (lambda (executable)
    `(unless (executable-find ,executable) ,(setup-quit)))
  :documentation "Stop the current setup when EXECUTABLE is unavailable."
  :repeatable 1)

(provide 'dream-setup)
;;; dream-setup.el ends here.
