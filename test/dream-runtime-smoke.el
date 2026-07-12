;;; dream-runtime-smoke.el --- Assert normal use produces no Lisp artifacts. -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst dream-runtime-smoke-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory load-file-name))))

(setq user-emacs-directory dream-runtime-smoke-root)

(defun dream-runtime-smoke--search-directory-p (directory)
  "Return non-nil when DIRECTORY should be searched for artifacts."
  (not (string= (file-name-nondirectory (directory-file-name directory))
                ".git")))

(defun dream-runtime-smoke--artifact-signatures ()
  "Return stable signatures for every ELC and ELN below the config root."
  (mapcar
   (lambda (file)
     (let ((attributes (file-attributes file 'string)))
       (list (file-relative-name file dream-runtime-smoke-root)
             (file-attribute-size attributes)
             (file-attribute-modification-time attributes))))
   (sort (directory-files-recursively
          dream-runtime-smoke-root "\\.el[cn]\\'" nil
          #'dream-runtime-smoke--search-directory-p)
         #'string<)))

(load (expand-file-name "early-init.el" dream-runtime-smoke-root) nil t)

(let ((before (dream-runtime-smoke--artifact-signatures)))
  (load (expand-file-name "init.el" dream-runtime-smoke-root) nil t)

  (let ((manifest (dream-startup-read-manifest)))
    (unless (and (dream-startup-manifest-compatible-p manifest)
                 (dream-startup-manifest-sources-current-p manifest))
      (error "Runtime smoke requires current artifacts; run `make config-build'")))

  (let ((once-idle-timer nil))
    (run-hooks 'emacs-startup-hook))
  (dream-run-hooks 'dream-first-input-hook
                   'dream-first-file-hook
                   'dream-first-buffer-hook)
  (with-temp-buffer
    (emacs-lisp-mode))
  (dolist (item (copy-sequence once--incremental-code))
    (unless (and (eq (car item) :feature) (featurep (cadr item)))
      (once--run item)))

  (accept-process-output nil 0.1)
  (require 'comp-run)
  (unless (and (null comp-files-queue)
               (zerop (hash-table-count comp-async-compilations)))
    (error "Native compilation work remains queued"))
  (unless (and (not native-comp-jit-compilation)
               (dream-runtime-trampolines-current-p)
               (cl-every
                (lambda (function)
                  (advice-member-p #'dream-runtime--guard-compilation
                                   function))
                dream-runtime--compiler-functions))
    (error "Runtime compilation policy is not fully active"))

  (let ((after (dream-runtime-smoke--artifact-signatures)))
    (unless (equal before after)
      (error "Normal runtime changed Lisp artifacts:\nbefore=%S\nafter=%S"
             before after))
    (princ (format "Dream runtime smoke: %d Lisp artifacts unchanged\n"
                   (length after)))))

;;; dream-runtime-smoke.el ends here.
