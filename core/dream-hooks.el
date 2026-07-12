;;; dream-hooks.el --- Deferred startup hooks for Dream Emacs. -*- lexical-binding: t; -*-

(require 'dream-lib)

(defvar dream-first-input-hook nil
  "Transient hooks run before the first user input.")

(defvar dream-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")

(defvar dream-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

(defvar dream-init-ui-hook nil
  "Transient hooks run once the first usable frame exists.")

(defvar dream-hooks--current nil
  "The hook variable `dream-run-hooks' is currently running.")

(defun dream-run-hook (hook)
  "Call HOOK (a function) and upgrade errors to `dream-hook-error'.
Meant for `run-hook-wrapped'; always returns nil so iteration
continues."
  (dream-log 3 "hook:%s: run %s" (or dream-hooks--current '*) hook)
  (condition-case-unless-debug error
      (funcall hook)
    (error
     (signal 'dream-hook-error (list hook error))))
  nil)

(defun dream-run-hooks (&rest hooks)
  "Run each hook variable in HOOKS, naming hook and function on error."
  (dolist (hook hooks)
    (condition-case-unless-debug error
        (let ((dream-hooks--current hook))
          (run-hook-wrapped hook #'dream-run-hook))
      (dream-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (nth 1 error) (nth 2 error)))
       (signal 'dream-hook-error (cons hook (cdr error)))))))

(defun dream-run-hook-on (hook-var trigger-hooks &optional predicate)
  "Run HOOK-VAR exactly once when any of TRIGGER-HOOKS first fires.
Fires only after Emacs has initialized (`after-init-time') and only
when PREDICATE (if any) returns non-nil.  HOOK-VAR is reset to nil
afterwards, and each chain attempts at most once even on error."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "dream-chain-%s-to-%s" hook-var hook)))
          running)
      (fset
       fn (lambda (&rest _)
            (when (and (not running)
                       after-init-time
                       (or (daemonp)
                           ;; Hooks may be let-bound to nil during internal
                           ;; batch operations; treat that as non-interactive.
                           (and (boundp hook) (symbol-value hook)))
                       (or (daemonp)
                           (null predicate)
                           (funcall predicate)))
              (setq running t)
              (dream-run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        ;; Daemon sessions skip the lazy-loading dance; load everything
        ;; when the first frame arrives.
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          ;; `find-file-hook' runs after the buffer is fully initialized,
          ;; which is too late; fire just before instead.
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))

(defun dream-hooks--run-init-ui (&rest _)
  "Run `dream-init-ui-hook' once, then disarm."
  (dream-run-hooks 'dream-init-ui-hook)
  (setq dream-init-ui-hook nil)
  (remove-hook 'window-setup-hook #'dream-hooks--run-init-ui)
  (remove-hook 'server-after-make-frame-hook #'dream-hooks--run-init-ui))

(defun dream-hooks-arm ()
  "Install the transient startup hook triggers for this session."
  (dream-run-hook-on 'dream-first-file-hook
                     '(find-file-hook dired-initial-position-hook))
  (dream-run-hook-on 'dream-first-input-hook '(pre-command-hook))
  (dream-run-hook-on 'dream-first-buffer-hook
                     '(find-file-hook window-buffer-change-functions
                       server-visit-hook)
                     (lambda ()
                       (not (equal (buffer-name) "*scratch*"))))
  (add-hook (if (daemonp) 'server-after-make-frame-hook 'window-setup-hook)
            #'dream-hooks--run-init-ui -100))

(unless noninteractive
  (dream-hooks-arm))

(provide 'dream-hooks)
;;; dream-hooks.el ends here.
