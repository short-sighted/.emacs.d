;; init.el --- Initialize Dream Emacs Configuration. -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;; load-path
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp/borg" user-emacs-directory))
(require 'borg)
(borg-initialize)

(eval-and-compile ; `borg'
  (defconst dream-local-directory (expand-file-name ".local" user-emacs-directory))
  (defconst dream-etc-directory (expand-file-name "etc" dream-local-directory))
  (defconst dream-var-directory (expand-file-name "var" dream-local-directory))

  (defvar single-autoload-path (expand-file-name "borg/autoload/" dream-etc-directory)
    "single autoload file.")

  (defun dream-collect-autoloads (file)
    "insert all enabled drone's autoloads file to a single file."
    (make-directory (file-name-directory file) 'parents)

    ;; cleanup obsolete autoloads file
    (dolist (f (directory-files single-autoload-path t "autoload-[0-9]+-[0-9]+\\.elc?\\'"))
      (unless (string= file f)
        (delete-file f)))

    (message "Generating single big autoload file.")
    (condition-case-unless-debug e
        (with-temp-file file
          (setq-local coding-system-for-write 'utf-8)
          (let ((standard-output (current-buffer))
                (print-quoted t)
                (print-level nil)
                (print-length nil)
                (home (expand-file-name "~"))
                path-list
                theme-path-list
                drones-path
                auto)
            (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                    ";; This file is generated from enabled drones.\n")

            ;; replace absolute path to ~
            (dolist (p load-path)
              ;; collect all drone's load-path
              (when (string-prefix-p (expand-file-name user-emacs-directory) (expand-file-name p))
                (push p drones-path))

              (if (string-prefix-p home p)
                  (push (concat "~" (string-remove-prefix home p)) path-list)
                (push p path-list)))

            (dolist (p custom-theme-load-path)
              (if (and (stringp p)
                       (string-prefix-p home p))
                  (push (concat "~" (string-remove-prefix home p)) theme-path-list)
                (push p theme-path-list)))

            (prin1 `(set `load-path ',path-list))
            (insert "\n")
            (print `(set `custom-theme-load-path ',(nreverse theme-path-list)))
            (insert "\n")

            ;; insert all drone's autoloads.el to this file
            (dolist (p drones-path)
              (when (file-exists-p p)
                (setq auto (car (directory-files p t ".*-autoloads.el\\'")))
                (when (and auto
                           (file-exists-p auto))
                  (insert-file-contents auto))))
            ;; remove all #$ load code
            (goto-char (point-min))
            (while (re-search-forward "\(add-to-list 'load-path.*#$.*\n" nil t)
              (replace-match ""))

            ;; write local variables region
            (goto-char (point-max))
            (insert  "\n"
                     "\n;; Local Variables:"
                     "\n;; version-control: never"
                     "\n;; no-update-autoloads: t"
                     "\n;; End:"
                     ))
          t)
      (error (delete-file file)
             (signal 'collect-autoload-error (list file e)))))

  (defun dream-initialize ()
    (let ((file (concat single-autoload-path
                        "autoload-"
                        (format-time-string
                         "%+4Y%m%d-%H%M%S"
                         (file-attribute-modification-time
                          (file-attributes "~/.emacs.d/.gitmodules")))
                        ".el")))
      (if (and (file-exists-p file)
               initial-window-system)
          (load file nil t)
        (require 'borg)
        (borg-initialize)
        (dream-collect-autoloads file))))

  (defun latest-file (path)
    "Get latest file (including directory) in PATH."
    (file-name-nondirectory (car (seq-find
                                  (lambda (x) (not (nth 1 x))) ; non-directory
                                  (sort
                                   (directory-files-and-attributes path 'full nil t)
                                   (lambda (x y) (time-less-p (nth 5 y) (nth 5 x))))))))

  (dream-initialize))


(let ((file-name-handler-alist nil))
  ;; Core
  (require 'dream-incremental-loading)
  (require 'dream-setup)
  (require 'dream-ui)
  (require 'dream-better-default)
  (require 'dream-editor)

  ;; Modules
  (require 'init-vc)
  (require 'init-completion)
  (require 'init-prog)
  (require 'init-lsp)
  (require 'init-check))

(provide 'init)
;;; init.el ends here.
