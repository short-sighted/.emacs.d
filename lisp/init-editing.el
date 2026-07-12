;;; init-editing.el --- Editing and persistence policy. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'dream-core)
(require 'dream-setup)

(cl-eval-when (compile)
  (require 'tramp)
  ;; `eshell-directory-name' lives in esh-mode.el, not eshell.el.
  (require 'esh-mode)
  (require 'transient)
  (require 'savehist)
  (require 'recentf)
  (require 'bookmark)
  (require 'hideshow))

(setq create-lockfiles nil
      make-backup-files nil
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist `(("." . ,dream-backup-directory))
      tramp-backup-directory-alist backup-directory-alist
      auto-save-default t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix dream-autosave-directory
      auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(file-name-concat dream-autosave-directory "tramp-\\2-") sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'"
         ,(file-name-concat dream-autosave-directory "\\2-") sha1))
      bookmark-default-file dream-bookmark-file
      tramp-persistency-file-name dream-tramp-file
      eshell-directory-name dream-eshell-directory
      transient-history-file (expand-file-name "history.el" dream-transient-directory)
      transient-levels-file (expand-file-name "levels.el" dream-transient-directory)
      transient-values-file (expand-file-name "values.el" dream-transient-directory)
      kill-do-not-save-duplicates t)

(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent 'complete)

(add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode))

(defun dream-editing--ensure-autosave-directory ()
  "Ensure the private autosave directory exists."
  (with-file-modes #o700
    (make-directory auto-save-list-file-prefix t)))

(defun dream-editing--guess-mode-after-save ()
  "Recompute the major mode after saving a fundamental-mode file."
  (when (eq major-mode 'fundamental-mode)
    (let ((buffer (or (buffer-base-buffer) (current-buffer))))
      (when (and (buffer-file-name buffer)
                 (eq buffer (window-buffer (selected-window))))
        (set-auto-mode)))))

(add-hook 'auto-save-hook #'dream-editing--ensure-autosave-directory)
(add-hook 'after-save-hook #'dream-editing--guess-mode-after-save)

(defun dream-editing--savehist-unpropertize ()
  "Strip text properties before persisting the kill ring."
  (setq kill-ring
        (mapcar #'substring-no-properties
                (cl-remove-if-not #'stringp kill-ring))))

(defun dream-editing--savehist-clean-registers ()
  "Keep only printable registers in savehist's temporary buffer."
  (setq-local register-alist
              (cl-remove-if-not #'savehist-printable register-alist)))

(setup savehist
  (:set savehist-file dream-savehist-file
        savehist-autosave-interval nil
        savehist-save-minibuffer-history t
        savehist-additional-variables
        '(kill-ring register-alist mark-ring global-mark-ring
          search-ring regexp-search-ring))
  (:once (list :before 'vertico-mode) (savehist-mode 1))
  (:when-loaded
    (add-hook 'savehist-save-hook #'dream-editing--savehist-unpropertize)
    (add-hook 'savehist-save-hook #'dream-editing--savehist-clean-registers)))

(setup recentf
  (:set recentf-auto-cleanup 'never
        recentf-max-menu-items 50
        recentf-max-saved-items 50
        recentf-save-file dream-recentf-file))

(add-hook 'dream-first-input-hook #'delete-selection-mode -90)
(add-hook 'dream-first-file-hook #'recentf-mode -90)
(add-hook 'dream-first-file-hook #'global-auto-revert-mode -80)
(add-hook 'dream-first-buffer-hook #'global-so-long-mode -90)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(with-eval-after-load 'hideshow
  (keymap-set hs-minor-mode-map "s-<tab>" #'hs-cycle))

(provide 'init-editing)
;;; init-editing.el ends here.
