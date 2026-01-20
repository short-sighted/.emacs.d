;; dream-lib.el --- Dream Emacs Library.  -*- lexical-binding: t; -*-

;; Requirements
(require 'cl-lib)

;; Helpers
(defun dream--ensure-list (obj)
  "Return OBJ as a list."
  (if (listp obj) obj (list obj)))

(defun dream--unquote (exp)
  "Return EXP unquoted."
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun dream--resolve-hook-forms (hooks)
  "Convert a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (let ((hook-list (dream--ensure-list (dream--unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

;; Font
(defun dream/font-available-p(font-name)
  "Check if not with FONT-NAME is avaiable."
  (find-font (font-spec :name font-name)))

(defmacro dream/add-hook (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)

Examples:
  (dream/add-hook 'text-mode-hook #'auto-fill-mode)
  (dream/add-hook '(text-mode-hook prog-mode-hook) :append #'my-fn)
  (dream/add-hook (text-mode prog-mode) (setq fill-column 80))
  (dream/add-hook '(one-mode-hook second-mode-hook)
    (defun my-hook-fn () ...))"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (unless hooks
    (error "dream/add-hook: HOOKS is required"))
  (when (keywordp hooks)
    (error "dream/add-hook: HOOKS must be a mode or hook symbol, not %S" hooks))
  (when (null rest)
    (error "dream/add-hook: at least one function or form is required"))
  (let* ((hook-forms (dream--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (when (or (null hook-forms)
              (not (cl-every #'symbolp hook-forms)))
      (error "dream/add-hook: HOOKS must resolve to symbols, got %S" hooks))
    (while (keywordp (car rest))
      (let ((kw (pop rest)))
        (pcase kw
          (:append (setq append-p t))
          (:depth  (if (null rest)
                       (error "dream/add-hook: :depth expects a number")
                     (setq depth (pop rest))))
          (:local  (setq local-p t))
          (:remove (setq remove-p t))
          (_ (error "dream/add-hook: unknown keyword %S" kw)))))
    (when (and depth (not (integerp depth)))
      (error "dream/add-hook: :depth expects an integer, got %S" depth))
    (when (null rest)
      (error "dream/add-hook: missing functions or forms"))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook ',(nreverse hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(provide 'dream-lib)
;;; dream-lib.el ends here.
