;;; run-tests.el --- Batch test entry point. -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/lang" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib" user-emacs-directory))
(load (expand-file-name "dream-core-test.el" (file-name-directory load-file-name))
      nil nil)

(ert-run-tests-batch-and-exit)
