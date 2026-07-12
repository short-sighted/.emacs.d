;;; dream-lib.el --- Small helpers for Dream Emacs. -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

;;; Errors

(define-error 'dream-error "An unexpected Dream Emacs error")
(define-error 'dream-hook-error "Error in a Dream Emacs startup hook"
              'dream-error)

;;; Logging

(defvar dream-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `dream-log' output completely.")

(defvar dream-log-level
  (if noninteractive
      3
    (if init-file-debug
        (if-let* ((level (getenv-internal "DEBUG"))
                  (level (if (string-empty-p level) 1 (string-to-number level)))
                  ((not (zerop level))))
            level
          2)
      0))
  "Verbosity of `dream-log' calls.
0 -- No logging at all.
1 -- Only warnings.
2 -- Warnings and notices.
3 -- Debug info, warnings, and notices.")

(defun dream--log (level text &rest args)
  "Emit TEXT formatted with ARGS at LEVEL into *Messages*.
Levels above `dream-log-level' stay out of the echo area."
  (let ((inhibit-message (if noninteractive
                             (not init-file-debug)
                           (> level dream-log-level))))
    (apply #'message
           (propertize (concat "* %.06f: " text) 'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           args)))

(defmacro dream-log (message &rest args)
  "Log MESSAGE (a format string applied to ARGS) when logging is on.
An integer in MESSAGE's position selects the level (default 2).
Expands to a guarded call, so ARGS are not evaluated while logging
is disabled."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message
                     (setq message (pop args)))
                 2)))
    `(when (and (not dream-inhibit-log)
                (or (not noninteractive)
                    (<= ,level dream-log-level)))
       (dream--log ,level ,message ,@args))))

(defun dream-unquote (expression)
  "Return EXPRESSION without quote or function wrappers."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe expression) '(quote function))
    (setq expression (cadr expression)))
  expression)

(defun dream--resolve-hook-forms (hooks)
  "Convert unquoted modes in HOOKS to hook symbols."
  (declare (pure t) (side-effect-free t))
  (let ((items (ensure-list (dream-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        items
      (mapcar (lambda (hook)
                (if (eq (car-safe hook) 'quote)
                    (cadr hook)
                  (intern (format "%s-hook" hook))))
              items))))

(defun dream-font-available-p (font-name)
  "Return non-nil when FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defmacro dream-add-hook (hooks &rest rest)
  "Add functions or forms in REST to HOOKS.
REST accepts :append, :local, :remove and :depth N before its functions."
  (declare (indent 1) (debug t))
  (let ((hook-forms (dream--resolve-hook-forms hooks))
        functions definitions append local remove depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append t))
        (:local (setq local t))
        (:remove (setq remove t))
        (:depth (setq depth (pop rest)))))
    (while rest
      (let* ((form (pop rest))
             (head (car-safe form)))
        (push
         (cond
          ((memq head '(function quote)) form)
          ((memq head '(defun cl-defun))
           (push form definitions)
           `#',(cadr form))
          (t
           (prog1 `(lambda (&rest _) ,form ,@rest)
             (setq rest nil))))
         functions)))
    `(progn
       ,@(nreverse definitions)
       (dolist (hook ',hook-forms)
         (dolist (function (list ,@(nreverse functions)))
           ,(if remove
                `(remove-hook hook function ,local)
              `(add-hook hook function ,(or depth append) ,local)))))))

(defmacro quiet!! (&rest body)
  "Evaluate BODY while suppressing messages and load/write notifications."
  (declare (indent 0))
  `(if init-file-debug
       (progn ,@body)
     (let ((inhibit-message t)
           (message-log-max nil)
           (standard-output #'ignore)
           (original-load (symbol-function 'load))
           (original-write-region (symbol-function 'write-region)))
       (cl-letf (((symbol-function 'message) #'ignore)
                 ((symbol-function 'load)
                  (lambda (file &optional noerror _nomessage nosuffix must-suffix)
                    (funcall original-load file noerror t nosuffix must-suffix)))
                 ((symbol-function 'write-region)
                  (lambda (start end filename &optional append visit lockname mustbenew)
                    (funcall original-write-region start end filename append
                             (or visit 'no-message) lockname mustbenew))))
         ,@body))))

(defmacro quiet! (&rest body)
  "Evaluate BODY without echo-area output."
  (declare (indent 0))
  `(if noninteractive
       (quiet!! ,@body)
     (let ((inhibit-message t) (save-silently t))
       ,@body)))

;;; Temporary bindings

(defmacro dream-letf (bindings &rest body)
  "Temporarily rebind functions, macros, or advice around BODY.
BINDINGS is a list of (PLACE VALUE) pairs as for `cl-letf*', or one
or more of these definition forms:

  (defun NAME (ARGS...) BODY...)     temporary function via `cl-letf'
  (defun* NAME (ARGS...) BODY...)    recursive function via `cl-labels'
  (defmacro NAME (ARGS...) BODY...)  temporary macro via `cl-macrolet'
  (defadvice FUNCTION WHERE ADVICE)  advice removed again afterwards"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq body
            (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice
               (cl-destructuring-bind (target where fn) rest
                 `(when-let* ((fn ,fn))
                    (advice-add ,target ,where fn)
                    (unwind-protect ,body (advice-remove ,target fn)))))
              (`defun
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  (cl-letf (((symbol-function #',(car rest))
                             (lambda ,(cadr rest) ,@(cddr rest))))
                    ,body)))
              (`defun*
               `(cl-labels ((,@rest)) ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

;;; Hook-local variables

(defun dream--setq-hook-forms (hooks rest &optional singles)
  "Return (VAR VAL HOOK FN) tuples for `dream-setq-hook' over HOOKS.
REST is the flat [SYM VAL]... list, or bare symbols when SINGLES."
  (unless (or singles (zerop (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list 'evenp (length rest))))
  (cl-loop with vars = (let ((args rest) vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (dream--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect (list var val hook
                                  (intern (format "dream--setq-%s-for-%s"
                                                  var mode))))))

(defmacro dream-setq-hook (hooks &rest var-vals)
  "Set buffer-local VAR-VALS whenever any of HOOKS run.
\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (dream--setq-hook-forms hooks var-vals)
            collect `(defun ,fn (&rest _) (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro dream-unsetq-hook (hooks &rest vars)
  "Remove the setters installed by `dream-setq-hook' for VARS.
\(fn HOOKS &rest SYM...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (dream--setq-hook-forms hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;;; Advice definers

(defmacro dream-defadvice (symbol arglist &optional docstring &rest body)
  "Define advice SYMBOL and attach it to the WHERE/PLACES pairs in BODY.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body))) where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro dream-undefadvice (symbol _arglist &optional docstring &rest body)
  "Remove advice SYMBOL from the WHERE/PLACES pairs in BODY.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body))) where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))

(provide 'dream-lib)
;;; dream-lib.el ends here.
