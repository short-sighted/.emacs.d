;;; run-tests.el --- Batch test entry point. -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))
(setq load-prefer-newer t)

(defvar dream-test-eln-directory (make-temp-file "dream-test-eln-" t))
(startup-redirect-eln-cache dream-test-eln-directory)
(setq native-comp-jit-compilation nil)
(add-hook 'kill-emacs-hook
          (lambda ()
            (when (file-directory-p dream-test-eln-directory)
              (delete-directory dream-test-eln-directory t))))

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(add-to-list 'load-path (file-name-directory load-file-name))
(load (expand-file-name "dream-core-test.el" (file-name-directory load-file-name))
      nil nil)

(ert-run-tests-batch-and-exit)
