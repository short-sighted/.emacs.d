;;; dream-hooks.el --- Deferred startup hooks for Dream Emacs. -*- lexical-binding: t; -*-

(require 'dream-core)

(defvar dream-first-input-hook nil
  "Transient hooks run before the first user input.")

(defvar dream-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")

(defvar dream-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

(defvar dream-init-ui-hook nil
  "Transient hooks run once the first usable frame exists.")

(cl-defstruct (dream-hooks--registration
               (:constructor dream-hooks--make-registration))
  hook-var predicate bindings attempted active)

(defvar dream-hooks--registrations (make-hash-table :test #'eq)
  "Active transient hook registrations, keyed by hook variable.")

(defvar dream-hooks--registration-order nil
  "Active transient hook registrations in installation order.")

(defvar dream-hooks--current nil
  "The hook variable `dream-run-hooks' is currently running.")

(defvar dream-hooks--failures nil
  "Failures collected while `dream-run-hooks' executes.")

(defvar dream-hooks--trigger nil
  "Trigger currently being evaluated by a lifecycle predicate.")

(defvar dream-hooks--trigger-args nil
  "Arguments supplied by `dream-hooks--trigger'.")

(defun dream-hooks--run-function (function)
  "Run hook FUNCTION, collecting failures without stopping iteration."
  (dream-log 3 "hook:%s: run %s" (or dream-hooks--current '*) function)
  (condition-case error
      (funcall function)
    (error (push (cons function error) dream-hooks--failures)))
  nil)

(defun dream-run-hooks (&rest hooks)
  "Run each hook variable in HOOKS and aggregate function failures."
  (let (failed-hooks)
    (dolist (hook hooks)
      (let ((dream-hooks--current hook)
            (dream-hooks--failures nil))
        (run-hook-wrapped hook #'dream-hooks--run-function)
        (when dream-hooks--failures
          (setq dream-hooks--failures (nreverse dream-hooks--failures))
          (dolist (failure dream-hooks--failures)
            (lwarn hook :error "Error running hook %S because: %s"
                   (car failure) (error-message-string (cdr failure))))
          (push (cons hook dream-hooks--failures) failed-hooks))))
    (when failed-hooks
      (setq failed-hooks (nreverse failed-hooks))
      (if (null (cdr failed-hooks))
          (signal 'dream-hook-error
                  (list :hook (caar failed-hooks)
                        :failures (cdar failed-hooks)))
        (signal 'dream-hook-error
                (list :hook (mapcar #'car failed-hooks)
                      :failures failed-hooks))))))

(defun dream-hooks--remove-registration (registration)
  "Remove every trigger installed for REGISTRATION."
  (when (dream-hooks--registration-active registration)
    (setf (dream-hooks--registration-active registration) nil)
    (dolist (binding (dream-hooks--registration-bindings registration))
      (pcase-let ((`(,kind ,target ,function) binding))
        (pcase kind
          ('hook (remove-hook target function))
          ('advice (advice-remove target function)))))
    (let ((hook-var (dream-hooks--registration-hook-var registration)))
      (when (eq (gethash hook-var dream-hooks--registrations) registration)
        (remhash hook-var dream-hooks--registrations)))
    (setq dream-hooks--registration-order
          (delq registration dream-hooks--registration-order))))

(defun dream-hooks-disarm (hook-var)
  "Remove every transient trigger registered for HOOK-VAR."
  (when-let* ((registration (gethash hook-var dream-hooks--registrations)))
    (dream-hooks--remove-registration registration)
    t))

(defun dream-hooks--signal-predicate-error (registration predicate error)
  "Disarm REGISTRATION and signal ERROR raised by PREDICATE."
  (setf (dream-hooks--registration-attempted registration) t)
  (dream-hooks--remove-registration registration)
  (signal 'dream-hook-error
          (list :hook (dream-hooks--registration-hook-var registration)
                :failures (list (cons predicate error)))))

(defun dream-hooks--fire (registration trigger &rest trigger-args)
  "Run REGISTRATION after one of its installed triggers fires."
  (when (and (dream-hooks--registration-active registration)
             (not (dream-hooks--registration-attempted registration))
             after-init-time)
    (let ((dream-hooks--trigger trigger)
          (dream-hooks--trigger-args trigger-args)
          (predicate (dream-hooks--registration-predicate registration))
          ready)
      (condition-case error
          (setq ready (or (daemonp) (null predicate) (funcall predicate)))
        (error
         (dream-hooks--signal-predicate-error registration predicate error)))
      (when ready
        (setf (dream-hooks--registration-attempted registration) t)
        (let (failure)
          (unwind-protect
              (condition-case error
                  (progn
                    (dream-run-hooks
                     (dream-hooks--registration-hook-var registration))
                    (set (dream-hooks--registration-hook-var registration) nil))
                (dream-hook-error (setq failure error)))
            (dream-hooks--remove-registration registration))
          (when failure
            (signal (car failure) (cdr failure))))))))

(defun dream-hooks--dispatch-daemon-frame (&rest _)
  "Fire every active lifecycle registration, then aggregate failures."
  (let (failures)
    (dolist (registration (copy-sequence dream-hooks--registration-order))
      (when (dream-hooks--registration-active registration)
        (condition-case error
            (dream-hooks--fire registration 'server-after-make-frame-hook)
          (error
           (push (cons (dream-hooks--registration-hook-var registration)
                       error)
                 failures)))))
    (when failures
      (signal 'dream-hook-error
              (list :hook 'server-after-make-frame-hook
                    :failures (nreverse failures))))))

(defun dream-run-hook-on (hook-var trigger-hooks &optional predicate)
  "Run HOOK-VAR exactly once when any of TRIGGER-HOOKS first fires.
Fires only after Emacs has initialized (`after-init-time') and only
when PREDICATE (if any) returns non-nil.  Success resets HOOK-VAR to
nil.  Success and failure both remove every installed trigger.
Return the function installed for the final trigger."
  (dream-hooks-disarm hook-var)
  (let ((registration
         (dream-hooks--make-registration
          :hook-var hook-var :predicate predicate :active t))
        last-function)
    (puthash hook-var registration dream-hooks--registrations)
    (setq dream-hooks--registration-order
          (append dream-hooks--registration-order (list registration)))
    (dolist (trigger trigger-hooks)
      (let ((function
             (lambda (&rest args)
               (apply #'dream-hooks--fire registration trigger args))))
        (if (eq trigger 'find-file-hook)
            (progn
              (advice-add 'after-find-file :before function '((depth . -101)))
              (push (list 'advice 'after-find-file function)
                    (dream-hooks--registration-bindings registration)))
          (add-hook trigger function -101)
          (push (list 'hook trigger function)
                (dream-hooks--registration-bindings registration)))
        (setq last-function function)))
    (when (daemonp)
      (add-hook 'server-after-make-frame-hook
                #'dream-hooks--dispatch-daemon-frame 90))
    last-function))

(defun dream-hooks--real-buffer-trigger-p ()
  "Return non-nil when the active trigger identifies a non-scratch buffer."
  (let* ((object (car dream-hooks--trigger-args))
         (buffers
          (cond
           ((and (eq dream-hooks--trigger 'window-buffer-change-functions)
                 (framep object))
            (mapcar #'window-buffer (window-list object 'no-minibuffer)))
           ((and (eq dream-hooks--trigger 'window-buffer-change-functions)
                 (windowp object))
            (list (window-buffer object)))
           (t (list (current-buffer))))))
    (cl-some (lambda (buffer)
               (and (buffer-live-p buffer)
                    (not (equal (buffer-name buffer) "*scratch*"))))
             buffers)))

(defun dream-hooks--arm ()
  "Install the transient startup hook triggers for this session."
  (dream-run-hook-on 'dream-first-file-hook
                     '(find-file-hook dired-initial-position-hook))
  (dream-run-hook-on 'dream-first-input-hook '(pre-command-hook))
  (dream-run-hook-on 'dream-first-buffer-hook
                     '(find-file-hook window-buffer-change-functions
                       server-visit-hook)
                     #'dream-hooks--real-buffer-trigger-p)
  (dream-run-hook-on 'dream-init-ui-hook '(window-setup-hook)))

(unless noninteractive
  (dream-hooks--arm))

(provide 'dream-hooks)
;;; dream-hooks.el ends here.
