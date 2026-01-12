;; -*- lexical-binding: t; no-byte-compile: t -*-
(setq load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(setq package-enable-at-startup nil)
