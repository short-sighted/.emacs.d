;;; dream-core-test.el --- Core tests for Dream Emacs. -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defvar dream-test-autoload-loaded nil)
(defvar dream-test-trigger-hook nil)
(defvar dream-test-buffer-hook nil)
(defvar web-mode-hook nil)
(defvar c-ts-mode-hook nil)
(defvar c++-ts-mode-hook nil)
(defvar rust-ts-mode-hook nil)

(add-to-list 'load-path (expand-file-name "../core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/once" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/once/once-setup" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/on" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/setup" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/lang" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lib" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../build" (file-name-directory load-file-name)))

(require 'dream-lib)

(ert-deftest dream-paths-use-cache-state-and-data-roots ()
  (require 'dream-paths)
  (should (equal dream-cache-directory
                 (expand-file-name ".local/cache/" user-emacs-directory)))
  (should (equal dream-state-directory
                 (expand-file-name ".local/state/" user-emacs-directory)))
  (should (equal dream-data-directory
                 (expand-file-name ".local/data/" user-emacs-directory))))

(ert-deftest dream-startup-does-not-compile-missing-artifacts ()
  (require 'dream-startup)
  (let ((native-comp-jit-compilation t)
        (byte-compile-called nil)
        (native-compile-called nil))
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (&rest _) (setq byte-compile-called t)))
              ((symbol-function 'native-compile)
               (lambda (&rest _) (setq native-compile-called t))))
      (dream-startup-disable-runtime-compilation)
      (should-not native-comp-jit-compilation)
      (should-not byte-compile-called)
      (should-not native-compile-called))))

(ert-deftest dream-startup-incremental-loading-waits-for-ui-in-daemon ()
  (require 'dream-startup)
  (let ((dream-startup--incremental-loading-started nil)
        (on-init-ui-hook nil)
        (started nil))
    (cl-letf (((symbol-function 'once-enable-incremental-loading)
               (lambda () (setq started t))))
      (dream-startup-arm-incremental-loading t)
      (should-not started)
      (run-hooks 'on-init-ui-hook)
      (should started))))

(ert-deftest dream-build-manifest-rejects-a-different-emacs-version ()
  (require 'dream-startup)
  (let ((manifest (list :schema 1
                        :emacs-version "0.0"
                        :native-version "invalid"
                        :eln-directory ".local/cache/eln/"
                        :lsp-use-plists "true")))
    (should-not (dream-startup-manifest-compatible-p manifest))))

(ert-deftest dream-build-manifest-detects-stale-owned-source ()
  (require 'dream-startup)
  (should-not
   (dream-startup-manifest-sources-current-p
    (list :sources '(("core/dream-startup.el" "not-the-current-hash"))))))

(ert-deftest dream-once-local-hook-cleans-up-in-the-origin-buffer ()
  (require 'once)
  (with-temp-buffer
    (let ((counter 0))
      (once-x-call (list :hooks (list :hook 'post-command-hook :local t))
        (lambda () (setq counter (1+ counter))))
      (run-hooks 'post-command-hook)
      (should (= counter 1))
      (run-hooks 'post-command-hook)
      (should (= counter 1)))))

(ert-deftest dream-once-package-condition-can-wait-for-all-features ()
  (require 'once)
  (let* ((counter 0)
         (directory (make-temp-file "dream-once-features-" t))
         (file-a (expand-file-name "dream-test-feature-a.el" directory))
         (file-b (expand-file-name "dream-test-feature-b.el" directory)))
    (unwind-protect
        (progn
          (with-temp-file file-a
            (insert ";;; -*- lexical-binding: t; -*-\n(provide 'dream-test-feature-a)\n"))
          (with-temp-file file-b
            (insert ";;; -*- lexical-binding: t; -*-\n(provide 'dream-test-feature-b)\n"))
          (once-x-call
              (list :packages 'dream-test-feature-a 'dream-test-feature-b
                    :check (lambda ()
                             (and (featurep 'dream-test-feature-a)
                                  (featurep 'dream-test-feature-b))))
            (lambda () (setq counter (1+ counter))))
          (load file-a nil nil)
          (should (= counter 0))
          (load file-b nil nil)
          (should (= counter 1)))
      (delete-directory directory t))))

(ert-deftest dream-setup-set-does-not-expand-to-customize ()
  (require 'dream-setup)
  (let ((expansion (macroexpand-all '(setup example
                                      (:set example-option 42)))))
    (should (equal expansion '(setq example-option 42)))
    (should-not (memq 'customize-set-variable (flatten-tree expansion)))))

(ert-deftest dream-setup-registers-once-keyword-aliases-before-loading-integration ()
  (require 'dream-setup)
  (should (eq (plist-get once-setup-keyword-aliases :once-x-require)
              :require-once))
  (should (eq (plist-get once-setup-keyword-aliases
                         :once-require-incrementally)
              :iload)))

(ert-deftest dream-setup-pruned-keywords-are-not-defined ()
  (require 'dream-setup)
  (dolist (keyword '(:opt :init :bind-map))
    (should-not (assq keyword setup-macros)))
  (dolist (keyword '(:global :hooks :after :load-after))
    (should (assq keyword setup-macros))))

(ert-deftest dream-autoloads-source-fallback-never-compiles ()
  (require 'dream-autoloads)
  (let* ((directory (make-temp-file "dream-autoload-test-" t))
         (dream-autoloads-directory directory)
         (dream-autoloads-file (expand-file-name "autoloads.el" directory))
         (dream-autoloads-state-file (expand-file-name "state.el" directory))
         (dream-autoloads-stale-check-delay nil)
         (dream-test-autoload-loaded nil)
         (compiled nil))
    (unwind-protect
        (progn
          (with-temp-file dream-autoloads-file
            (insert ";;; -*- lexical-binding: t; -*-\n"
                    "(setq dream-test-autoload-loaded 'source)\n"))
          (cl-letf (((symbol-function 'byte-compile-file)
                     (lambda (&rest _) (setq compiled t)))
                    ((symbol-function 'native-compile)
                     (lambda (&rest _) (setq compiled t))))
            (dream-autoloads-initialize)
            (should (eq dream-test-autoload-loaded 'source))
            (should-not compiled)))
      (delete-directory directory t))))

(ert-deftest dream-config-registers-without-eagerly-loading-large-packages ()
  (require 'dream-paths)
  (require 'dream-setup)
  (let ((on-first-input-hook nil)
        (on-first-file-hook nil)
        (on-first-buffer-hook nil)
        (on-init-ui-hook nil)
        (prog-mode-hook nil)
        (text-mode-hook nil)
        (conf-mode-hook nil)
        (web-mode-hook nil)
        (c-ts-mode-hook nil)
        (c++-ts-mode-hook nil)
        (rust-ts-mode-hook nil)
        (lsp-mode-hook nil))
    (dolist (feature '(init-editing init-ui init-completion init-lsp init-lang
                       init-spell init-vc init-tools init-llm))
      (require feature))
    (should (equal (alist-get 'file completion-category-overrides)
                   '((styles orderless partial-completion))))
    (dolist (feature '(consult corfu lsp-mode magit gptel epkg))
      (should-not (featurep feature)))
    (dolist (hook '(web-mode-hook c-ts-mode-hook c++-ts-mode-hook rust-ts-mode-hook))
      (should (memq #'lsp-deferred (symbol-value hook))))
    (dolist (feature '(init-editing init-ui init-completion init-lsp
                       init-lang-programming init-lang-cpp init-lang-rust
                       init-lang-web init-lang init-spell init-vc init-tools
                       init-llm))
      (should (eq (require-with-check feature) feature)))))

(ert-deftest dream-build-file-set-scans-lisp-tree ()
  (require 'dream-compile)
  (let ((files (mapcar (lambda (file)
                         (file-relative-name file user-emacs-directory))
                       (dream-build-config-files))))
    (should (member "init.el" files))
    (should (member "core/dream-startup.el" files))
    (should (member "lisp/init-lsp.el" files))
    (should-not (member "early-init.el" files))
    (should (equal (car (last files)) "init.el"))
    (should (member "build/dream-compile.el"
                    (mapcar (lambda (file)
                              (file-relative-name file user-emacs-directory))
                            (dream-build-tool-files))))))

(ert-deftest dream-build-owned-lisp-scan-matches-load-path-boundaries ()
  (require 'dream-compile)
  (let ((base (make-temp-file "dream-build-scan-" t)))
    (unwind-protect
    (let ((user-emacs-directory (file-name-as-directory base)))
      (make-directory (expand-file-name "lib/ext" base) t)
      (make-directory (expand-file-name "lisp/lang" base) t)
      (make-directory (expand-file-name "lisp/ignored" base) t)
      (with-temp-file (expand-file-name "lisp/ignored/.nosearch" base))
      (with-temp-file (expand-file-name "lib/ext/dream-x.el" base))
      (with-temp-file (expand-file-name "lisp/init-a.el" base))
      (with-temp-file (expand-file-name "lisp/lang/init-lang.el" base))
      (with-temp-file (expand-file-name "lisp/.#init-a.el" base))
      (with-temp-file (expand-file-name "lisp/scratch.el" base))
      (with-temp-file (expand-file-name "lisp/ignored/init-hidden.el" base))
      (let ((files (mapcar (lambda (file) (file-relative-name file base))
                           (dream-build-owned-lisp-files))))
        (should (equal files '("lib/ext/dream-x.el"
                               "lisp/init-a.el"
                               "lisp/lang/init-lang.el")))))
      (delete-directory base t))))

(ert-deftest dream-paths-add-load-paths-is-repeatable-and-honors-nosearch ()
  (require 'dream-paths)
  (let ((base (make-temp-file "dream-load-path-" t)))
    (unwind-protect
    (let ((user-emacs-directory (file-name-as-directory base))
          (load-path load-path)
          (load-path-filter--cache 'stale))
      (make-directory (expand-file-name "lib/ext" base) t)
      (make-directory (expand-file-name "lisp/lang" base) t)
      (make-directory (expand-file-name "lisp/ignored" base) t)
      (with-temp-file (expand-file-name "lisp/ignored/.nosearch" base))
      (dream-paths-add-load-paths)
      (dream-paths-add-load-paths)
      (dolist (directory '("lib" "lib/ext" "lisp" "lisp/lang"))
        (let ((absolute (directory-file-name
                         (expand-file-name directory base))))
          (should (= 1 (seq-count (lambda (item) (equal item absolute))
                                  load-path)))))
      (should-not (member (directory-file-name
                           (expand-file-name "lisp/ignored" base))
                          load-path))
      (should-not load-path-filter--cache))
      (delete-directory base t))))

(ert-deftest dream-language-lsp-integration-is-order-independent ()
  (when (featurep 'init-lsp)
    (unload-feature 'init-lsp t))
  (let ((rust-ts-mode-hook nil))
    (load (expand-file-name "lisp/lang/init-lang-rust.el"
                            user-emacs-directory)
          nil t)
    (should-not (memq #'lsp-deferred rust-ts-mode-hook))
    (load (expand-file-name "lisp/init-lsp.el" user-emacs-directory) nil t)
    (should (= 1 (seq-count (lambda (function)
                             (eq function #'lsp-deferred))
                           rust-ts-mode-hook)))))

(ert-deftest dream-once-installs-a-hook-once-without-making-it-one-buffer-only ()
  (require 'once)
  (let ((dream-test-trigger-hook nil)
        (dream-test-buffer-hook nil)
        (calls 0))
    (once (list :hooks 'dream-test-trigger-hook)
      (add-hook 'dream-test-buffer-hook (lambda () (cl-incf calls))))
    (run-hooks 'dream-test-trigger-hook)
    (run-hooks 'dream-test-trigger-hook)
    (should (= 1 (length dream-test-buffer-hook)))
    (with-temp-buffer (run-hooks 'dream-test-buffer-hook))
    (with-temp-buffer (run-hooks 'dream-test-buffer-hook))
    (should (= calls 2))))

(ert-deftest dream-build-rejects-shadowing-owned-library-names ()
  (require 'dream-compile)
  (should (fboundp 'dream-build--assert-unique-library-names))
  (should-error
   (dream-build--assert-unique-library-names
    '("/tmp/lib/dream-x.el" "/tmp/lib/nested/dream-x.el"))
   :type 'error))

(ert-deftest dream-emacs-31-user-lisp-auto-scrape-is-disabled ()
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (should-not user-lisp-auto-scrape))

(ert-deftest dream-extensions-load-at-declared-boundaries ()
  (when (featurep 'dream-eldoc)
    (unload-feature 'dream-eldoc t))
  (require 'init-lang)
  (should-not (featurep 'dream-flymake))
  (should (featurep 'eldoc))
  (should-not (featurep 'dream-eldoc))
  (require 'flymake)
  (should (featurep 'dream-flymake))
  (should (fboundp 'dream-flymake-next-error))
  (with-temp-buffer
    (eldoc-mode 1))
  (should (featurep 'dream-eldoc)))

(ert-deftest dream-flymake-error-row-boundary-matches-emacs-31-metadata ()
  (require 'dream-flymake)
  (with-temp-buffer
    (tabulated-list-mode)
    (setq tabulated-list-format [("Type" 8)]
          tabulated-list-entries
          `(((:severity ,(warning-numeric-level :error)) ["error"])))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (goto-char (point-min))
    (should (dream-flymake--error-row-p))))

(ert-deftest dream-build-manifest-round-trips-runtime-contract ()
  (require 'dream-compile)
  (let* ((directory (make-temp-file "dream-manifest-test-" t))
         (dream-build-manifest-file (expand-file-name "manifest.el" directory)))
    (unwind-protect
        (progn
          (dream-build-write-manifest nil)
          (let ((manifest (dream-startup-read-manifest)))
            (should (dream-startup-manifest-compatible-p manifest))
            (should (eq (plist-get manifest :native) nil))))
      (delete-directory directory t))))

(ert-deftest dream-full-build-uses-config-native-policy ()
  (require 'dream-compile)
  (let (config-args autoloads-called)
    (cl-letf (((symbol-function 'dream-build-autoloads)
               (lambda () (setq autoloads-called t)))
              ((symbol-function 'dream-build-config)
               (lambda (&rest args) (setq config-args args))))
      (dream-build-after-borg-rebuild t)
      (should autoloads-called)
      (should (equal config-args
                     (list dream-build-default-config-native t))))))

(ert-deftest dream-config-byte-build-removes-only-owned-native-artifacts ()
  (require 'dream-compile)
  (let ((directory (make-temp-file "dream-owned-eln-" t)))
    (unwind-protect
        (let* ((dream-eln-directory (file-name-as-directory directory))
               (owned (expand-file-name "owned.eln" directory))
               (third-party (expand-file-name "third-party.eln" directory)))
          (with-temp-file owned)
          (with-temp-file third-party)
          (cl-letf (((symbol-function 'dream-build-config-files)
                     (lambda () '("/tmp/owned.el")))
                    ((symbol-function 'comp-el-to-eln-filename)
                     (lambda (_file) owned)))
            (dream-build-clean-config-native))
          (should-not (file-exists-p owned))
          (should (file-exists-p third-party)))
      (delete-directory directory t))))

(ert-deftest dream-build-isolated-compile-rejects-undeclared-free-variables ()
  (require 'dream-compile)
  (let ((file (make-temp-file
               "dream-isolation-bad-" nil ".el"
               ";;; bad.el --- test -*- lexical-binding: t; -*-\n(setq dream-test-undeclared-variable 42)\n(provide 'dream-isolation-bad)\n")))
    (unwind-protect
        (should-error (dream-build--check-isolated-files (list file)))
      (delete-file file))))

(ert-deftest dream-build-isolated-compile-accepts-self-declared-context ()
  (require 'dream-compile)
  (let ((file (make-temp-file
               "dream-isolation-good-" nil ".el"
               ";;; good.el --- test -*- lexical-binding: t; -*-\n(defvar dream-test-declared-variable)\n(setq dream-test-declared-variable 42)\n(provide 'dream-isolation-good)\n")))
    (unwind-protect
        (should (dream-build--check-isolated-files (list file)))
      (delete-file file))))

(ert-deftest dream-declare-function-forms-always-name-their-file ()
  (require 'dream-compile)
  (should (fboundp 'dream-build-check-declare))
  (dolist (file (append (dream-build-config-files) (dream-build-tool-files)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (eq (car-safe form) 'declare-function)
                ;; Rule 5 requires FILE so check-declare can verify the claim.
                (should (stringp (nth 2 form))))))
        (end-of-file nil)))))

(ert-deftest dream-lsp-sets-real-lsp-variables ()
  (require 'init-lsp)
  (should (boundp 'lsp-keep-workspace-alive))
  (should-not lsp-keep-workspace-alive)
  (should-not (boundp 'lsp-keep-workspace)))

(ert-deftest dream-lang-rust-registers-clippy-autoload ()
  (skip-unless (executable-find "cargo"))
  (require 'init-lang-rust)
  (should (fboundp 'flymake-clippy-setup-backend))
  (should-not (featurep 'flymake-clippy)))

(ert-deftest dream-error-hierarchy-is-catchable-as-dream-error ()
  (require 'dream-lib)
  (should (equal (get 'dream-hook-error 'error-conditions)
                 '(dream-hook-error dream-error error)))
  (should-error (signal 'dream-hook-error '(test)) :type 'dream-error))

(ert-deftest dream-log-honors-inhibit-flag-and-level ()
  (require 'dream-lib)
  (let ((dream-inhibit-log t))
    (should-not (dream-log "never emitted")))
  (let ((dream-inhibit-log nil)
        (dream-log-level 3)
        (captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args) (push (cons format args) captured))))
      (dream-log 3 "visible %s" 'entry)
      (dream-log 2 "also visible"))
    (should (= 2 (length captured)))))

(provide 'dream-core-test)
;;; dream-core-test.el ends here.
