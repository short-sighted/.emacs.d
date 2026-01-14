;; dream-incremental-loading.el --- Initialize incremental loading configurations. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Loading pacakges incrementally.
;;
;;; Code:

;; Customizations
(defgroup dream-iloader nil
  "Load pacakges incrementally."
  :group 'dream)

(defvar dream-incremental-packages '()
  "A list of packages to load incrementally after startup. Any large packages
  here may cause noticeable pauses, so it's recommended you break them up into
  sub-packages. For example, `org' is comprised of many packages, and can be
  broken up into:

    (dream-load-packages-incrementally
     \='(calendar find-func format-spec org-macs org-compat
       org-faces org-entities org-list org-pcomplete org-src
       org-footnote org-macro ob org org-clock org-agenda
       org-capture))

  Incremental loading does not occur in daemon sessions (they are
  loaded immediately at startup).")

(defcustom dream-incremental-first-idle-timer (if (daemonp) 0 1.5)
  "How long (in idle seconds) until incremental loading starts.

 Set this to nil to disable incremental loading. Set this to 0 to
load all incrementally deferred packages immediately at
`emacs-startup-hook'."
  :group 'dream-iloader
  :type 'number)

(defcustom dream-incremental-idle-timer 1.5
  "How long (in idle seconds) in between incrementally loading packages."
  :group 'dream-iloader
  :type 'number)

;;;; Loading packages incrementally.
(defun dream-load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

If NOW is non-nil, load PACKAGES incrementally,
in `dream-incremental-idle-timer' intervals."
  (let ((gc-cons-threshold most-positive-fixnum))
    (if (not now)
        (cl-callf append dream-incremental-packages packages)
      (while packages
        (let ((req (pop dream-incremental-packages)))
          (condition-case-unless-debug e
              (or (not
                   (while-no-input
                     ;; (message "Loading %s (%d left)" req (length dream-incremental-packages))
                     ;; If `default-directory' doesn't exist or is
                     ;; unreadable, Emacs throws file errors.
                     (let ((default-directory user-emacs-directory)
                           (inhibit-message t)
                           (file-name-handler-alist
                            (list (rassq 'jka-compr-handler file-name-handler-alist))))
                       (require req nil t)
                       nil)))
                  (push req dream-incremental-packages))
            (error
             (message "Error: failed to incrementally load %S because: %s" req e)
             (setq dream-incremental-packages nil)))
          (when packages
            (run-with-idle-timer dream-incremental-idle-timer
                                 nil #'dream-load-packages-incrementally
                                 dream-incremental-packages t)
            (setq packages nil)))))))

(defun dream-load-packages-incrementally-h ()
  "Begin incrementally loading packages in `dream-incremental-packages'.

If this is a daemon session, load them all immediately instead."
  (when (numberp dream-incremental-first-idle-timer)
    (if (zerop dream-incremental-first-idle-timer)
        (mapc #'require (cdr dream-incremental-packages))
      (run-with-idle-timer dream-incremental-first-idle-timer
                           nil #'dream-load-packages-incrementally
                           dream-incremental-packages t))))

(add-hook 'emacs-startup-hook #'dream-load-packages-incrementally-h)

(provide 'dream-incremental-loading)
;;; dream-incremntal-loading.el ends here.
