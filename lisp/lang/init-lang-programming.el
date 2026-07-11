;;; init-lang-programming.el --- Programming foundations. -*- lexical-binding: t; -*-

(require 'dream-paths)
(require 'dream-setup)

(cl-eval-when (compile)
  (require 'treesit)
  (require 'flymake)
  (require 'yasnippet))

(setq treesit-enabled-modes t
      treesit-auto-install-grammar 'ask
      treesit-extra-load-path (list dream-treesit-directory)
      treesit-font-lock-level 4)

(setup eldoc
  (:set eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-prefer-doc-buffer 'maybe)
  (:require-once (list :hooks 'eldoc-mode-hook) 'dream-eldoc))

(setup yasnippet
  (:iload -60 eldoc easymenu help-mode -50 yasnippet)
  (:once (list :hooks (list :hook 'prog-mode-hook :depth -90))
    (yas-global-mode 1))
  (:autoload yas-minor-mode-on yas-expand yas-expand-snippet
             yas-lookup-snippet yas-insert-snippet yas-new-snippet
             yas-visit-snippet-file yas-activate-extra-mode
             yas-deactivate-extra-mode yas-maybe-expand-abbrev-key-filter))

(defun dream-diagnostics-elisp-load-path (function &rest args)
  "Call Elisp Flymake FUNCTION with the full configured load path."
  (let ((elisp-flymake-byte-compile-load-path
         (append elisp-flymake-byte-compile-load-path load-path)))
    (apply function args)))

(setup flymake
  (:hook-into prog-mode)
  (:global "C-c f" flymake-show-buffer-diagnostics)
  (:set flymake-no-changes-timeout nil
        flymake-fringe-indicator-position 'right-fringe
        flymake-margin-indicator-position 'right-margin)
  (:advice elisp-flymake-byte-compile
           :around dream-diagnostics-elisp-load-path)
  (:also-load dream-flymake))

(provide 'init-lang-programming)
;;; init-lang-programming.el ends here.
