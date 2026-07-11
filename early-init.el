;;; early-init.el --- Early initialization for Dream Emacs. -*- lexical-binding: t; no-byte-compile: t; -*-

(when (version< emacs-version "31")
  (error "Dream Emacs requires Emacs 31 or newer; running %s" emacs-version))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      load-prefer-newer nil
      package-enable-at-startup nil
      native-comp-jit-compilation nil
      user-lisp-auto-scrape nil)

(setenv "LSP_USE_PLISTS" "true")
(startup-redirect-eln-cache ".local/cache/eln/")
(setq load-path-filter-function #'load-path-filter-cache-directory-files)

(setq auto-mode-case-fold nil
      fast-but-imprecise-scrolling t
      ffap-machine-p-known 'reject
      frame-inhibit-implied-resize t
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      pgtk-wait-for-event-timeout 0.001
      read-process-output-max (* 64 1024)
      process-adaptive-read-buffering nil
      redisplay-skip-fontification-on-input t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here.
