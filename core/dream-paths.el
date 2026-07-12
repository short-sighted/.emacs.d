;;; dream-paths.el --- Filesystem layout for Dream Emacs. -*- lexical-binding: t; -*-

(defgroup dream nil
  "Dream Emacs configuration."
  :group 'environment)

(defconst dream-local-directory
  (file-name-as-directory (expand-file-name ".local" user-emacs-directory)))
(defconst dream-cache-directory
  (file-name-as-directory (expand-file-name "cache" dream-local-directory)))
(defconst dream-state-directory
  (file-name-as-directory (expand-file-name "state" dream-local-directory)))
(defconst dream-data-directory
  (file-name-as-directory (expand-file-name "data" dream-local-directory)))

(defconst dream-autoloads-directory
  (file-name-as-directory (expand-file-name "borg" dream-cache-directory)))
(defconst dream-autoloads-file
  (expand-file-name "autoloads.el" dream-autoloads-directory))
(defconst dream-autoloads-state-file
  (expand-file-name "autoloads-state.el" dream-autoloads-directory))
(defconst dream-build-manifest-file
  (expand-file-name "build-manifest.el" dream-cache-directory))
(defconst dream-environment-file
  (expand-file-name "environment.el" dream-state-directory))
(defconst dream-eln-directory
  (file-name-as-directory (expand-file-name "eln" dream-cache-directory)))

(defconst dream-backup-directory
  (file-name-as-directory (expand-file-name "recovery/backup" dream-state-directory)))
(defconst dream-autosave-directory
  (file-name-as-directory (expand-file-name "recovery/autosave" dream-state-directory)))
(defconst dream-savehist-file (expand-file-name "savehist" dream-state-directory))
(defconst dream-recentf-file (expand-file-name "recentf" dream-state-directory))
(defconst dream-bookmark-file (expand-file-name "bookmarks" dream-state-directory))
(defconst dream-tramp-file (expand-file-name "tramp" dream-state-directory))
(defconst dream-eshell-directory
  (file-name-as-directory (expand-file-name "eshell" dream-state-directory)))
(defconst dream-transient-directory
  (file-name-as-directory (expand-file-name "transient" dream-state-directory)))
(defconst dream-lsp-session-file (expand-file-name "lsp-session" dream-state-directory))
(defconst dream-benchmark-directory
  (file-name-as-directory (expand-file-name "benchmarks" dream-state-directory)))

(defconst dream-epkg-directory
  (file-name-as-directory (expand-file-name "epkg" dream-data-directory)))
(defconst dream-lsp-server-directory
  (file-name-as-directory (expand-file-name "lsp" dream-data-directory)))
(defconst dream-treesit-directory
  (file-name-as-directory (expand-file-name "tree-sitter" dream-data-directory)))

(defun dream-paths-reset-load-path-cache (&rest _)
  "Clear Emacs 31's cached directory inventory after files change."
  ;; Emacs 31 exposes the cache feature but no public invalidation function.
  (when (boundp 'load-path-filter--cache)
    (setq load-path-filter--cache nil)))

(defun dream-paths-add-load-paths ()
  "Add owned library trees to `load-path' using Emacs startup rules."
  (dolist (name '("lib" "lisp"))
    (let ((directory (directory-file-name
                      (expand-file-name name user-emacs-directory))))
      (when (file-directory-p directory)
        (add-to-list 'load-path directory)
        (let ((default-directory (file-name-as-directory directory))
              (normal-top-level-add-subdirs-inode-list nil))
          (normal-top-level-add-subdirs-to-load-path)))))
  (setq load-path (delete-dups load-path))
  (dream-paths-reset-load-path-cache))

(defun dream-paths-searchable-subdirectory-p (directory)
  "Return non-nil when DIRECTORY follows Emacs load-path search rules."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (and (string-match-p "\\`[[:alnum:]]" name)
         (not (member name '("RCS" "CVS" "rcs" "cvs")))
         (not (file-exists-p (expand-file-name ".nosearch" directory))))))

(defun dream-paths-initialize ()
  "Create the writable roots used by Dream Emacs."
  (dolist (directory (list dream-cache-directory
                           dream-state-directory
                           dream-data-directory
                           dream-autoloads-directory
                           dream-eln-directory
                           dream-backup-directory
                           dream-autosave-directory
                           dream-eshell-directory
                           dream-transient-directory
                           dream-benchmark-directory
                           dream-lsp-server-directory
                           dream-treesit-directory))
    (make-directory directory t)))

(provide 'dream-paths)
;;; dream-paths.el ends here.
