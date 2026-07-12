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

(provide 'dream-lib)
;;; dream-lib.el ends here.
