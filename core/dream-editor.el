;; dream-editor.el --- Initialize dream editor configurations.  -*- lexical-binding: t; -*-


;; Favor spaces over tabs. Pls dun h8, but I think spaces (and 4 of them) is a
;; more consistent default than 8-space tabs. It can be changed on a per-mode
;; basis anyway (and is, where tabs are the canonical style, like `go-mode').
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

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
