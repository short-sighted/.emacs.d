;; dream-editor.el --- Initialize dream editor configurations.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;
;;; File Handling

;; Don't generate backups or lockfiles. While auto-save maintains a copy so long
;; as a buffer is unsaved, backups create copies once, when the file is first
;; written, and never again until it is killed and reopened. This is better
;; suited to version control, and I don't want world-readable copies of
;; potentially sensitive material floating around our filesystem.
(setq create-lockfiles nil
     make-backup-files nil
     version-control t
     backup-by-copying t
     delete-old-versions t
     kept-old-versions 5
     kept-new-versions 5
     backup-directory-alist `(("." . ,(expand-file-name "backup" dream-var-directory)))
     tramp-backup-directory-alist backup-directory-alist)

;; But turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setq auto-save-default t
      ;; Don't auto-disable auto-save after deleting big chunks. This defeats
      ;; the purpose of a failsafe. This adds the risk of losing the data we
      ;; just deleted, but I believe that's VCS's jurisdiction, not ours.
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (expand-file-name "autosave/" dream-var-directory)
      ;; This resolves two issue while ensuring auto-save files are still
      ;; reasonably recognizable at a glance:
      ;;
      ;; 1. Emacs generates long file paths for its auto-save files; long =
      ;;    `auto-save-list-file-prefix' + `buffer-file-name'. If too long, some
      ;;    filesystems (*cough*Windows) will murder your family. `sha1'
      ;;    compresses the path into a ~40 character hash (Emacs 28+ only)!
      ;; 2. The default transform rule writes TRAMP auto-save files to
      ;;    `temporary-file-directory', which TRAMP doesn't like! It'll prompt
      ;;    you about it every time an auto-save file is written, unless
      ;;    `tramp-allow-unsafe-temporary-files' is set. A more sensible default
      ;;    transform is better:
      auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(file-name-concat auto-save-list-file-prefix "tramp-\\2-") sha1)
        ("\\`/\\([^/]+/\\)*\\([^/]+\\)\\'" ,(file-name-concat auto-save-list-file-prefix "\\2-") sha1)))

(dream/add-hook 'auto-save-hook
  (defun dream-ensure-auto-save-prefix-exists-h ()
    (with-file-modes #o700
      (make-directory auto-save-list-file-prefix t))))

(dream/add-hook 'after-save-hook
  (defun dream-guess-mode-h ()
    "Guess major mode when saving a file in `fundamental-mode'.

Likely, something has changed since the buffer was opened. e.g. A shebang line
or file path may exist now."
    (when (eq major-mode 'fundamental-mode)
      (let ((buffer (or (buffer-base-buffer) (current-buffer))))
        (and (buffer-file-name buffer)
             (eq buffer (window-buffer (selected-window)))
             (set-auto-mode)
             (not (eq major-mode 'fundamental-mode)))))))

;;
;;; Formatting

;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like `go-mode').
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;;
;;; Clipboard / kill-ring

;; Cull duplicates in the kill ring to reduce bloat and make the kill ring
;; easier to peruse (with `counsel-yank-pop').
(setq kill-do-not-save-duplicates t)

;;
;;; Extra file extensions to support

(add-to-list 'auto-mode-alist '("/LICENSE\\'" . text-mode))


;; Bookmarks default file
(setq bookmark-default-file (expand-file-name "bookmarks" dream-var-directory))

;; `savehist-mode'
(setup savehist
  (defun dream-savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
	  (mapcar #'substring-no-properties
		  (cl-remove-if-not #'stringp kill-ring))))
  (defun dream-savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
		(cl-remove-if-not #'savehist-printable register-alist)))
  (:once (list :before 'vertico-mode)
    (savehist-mode))
  (:opt* savehist-file (expand-file-name "savehist" dream-var-directory)
	 savehist-autosave-interval nil
	 savehist-save-minibuffer-history t
	 savehist-additional-variables '(kill-ring
					 register-alist
					 mark-ring global-mark-ring
					 search-ring regexp-search-ring))
  (:after savehist
    (:hooks
     savehist-save-hook dream-savehist-unpropertize-variables-h
     savehist-save-hook dream-savehist-remove-unprintable-registers-h)))

;; `so-long-mode'
(setup so-long
  (:hooks after-init-hook global-so-long-mode)
  (:after so-long
    (if (fboundp 'buffer-line-statistics)
	(unless (featurep 'native-compile)
	  (setq so-long-threshold 5000))
      (setq so-long-threshold 400))))

(provide 'dream-editor)
;;; dream-editor.el ends here.
