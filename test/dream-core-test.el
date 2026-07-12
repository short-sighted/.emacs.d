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
(defvar dream-test-transient-hook nil)
(defvar dream-test-transient-two-hook nil)
(defvar dream-test-trigger-a-hook nil)
(defvar dream-test-trigger-b-hook nil)
(defvar dream-test-mode-hook nil)
(defvar dream-test-local-var)

(add-to-list 'load-path (expand-file-name "../core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/once" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/once/once-setup" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../site-lisp/setup" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/lang" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lib" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../build" (file-name-directory load-file-name)))

(require 'dream-core)

(ert-deftest dream-paths-use-cache-state-and-data-roots ()
  (require 'dream-paths)
  (should (equal dream-cache-directory
                 (expand-file-name ".local/cache/" user-emacs-directory)))
  (should (equal dream-state-directory
                 (expand-file-name ".local/state/" user-emacs-directory)))
  (should (equal dream-data-directory
                 (expand-file-name ".local/data/" user-emacs-directory))))

(ert-deftest dream-startup-does-not-compile-missing-artifacts ()
  (require 'dream-runtime)
  (let ((native-comp-jit-compilation t)
        (native-comp-enable-subr-trampolines
         native-comp-enable-subr-trampolines)
        (byte-compile-called nil)
        (native-compile-called nil))
    (cl-letf (((symbol-function 'byte-compile-file)
               (lambda (&rest _) (setq byte-compile-called t)))
              ((symbol-function 'native-compile)
               (lambda (&rest _) (setq native-compile-called t))))
      (dream-runtime--disable-automatic-compilation)
      (should-not native-comp-jit-compilation)
      (should-not byte-compile-called)
      (should-not native-compile-called))))

(ert-deftest dream-startup-incremental-loading-waits-for-ui-in-daemon ()
  (require 'dream-startup)
  (let ((dream-startup--incremental-loading-started nil)
        (dream-init-ui-hook nil)
        (started nil))
    (cl-letf (((symbol-function 'once-enable-incremental-loading)
               (lambda () (setq started t))))
      (dream-startup-arm-incremental-loading t)
      (should-not started)
      (run-hooks 'dream-init-ui-hook)
      (should started))))

(ert-deftest dream-build-manifest-rejects-a-different-emacs-version ()
  (require 'dream-startup)
  (let ((manifest (list :schema 1
                        :emacs-version "0.0"
                        :native-version "invalid"
                        :eln-directory ".local/cache/eln/"
                        :lsp-use-plists "true")))
    (should-not (dream-startup-manifest-compatible-p manifest))))

(ert-deftest dream-build-manifest-rejects-a-different-system-build ()
  (require 'dream-compile)
  (cl-letf (((symbol-function 'dream-runtime-trampolines-current-p)
             (lambda () t)))
    (let ((manifest (dream-build-manifest nil nil)))
      (setf (plist-get manifest :system-configuration)
            "different-system-configuration")
      (should-not (dream-startup-manifest-compatible-p manifest)))))

(ert-deftest dream-build-manifest-rejects-missing-required-fields ()
  (require 'dream-startup)
  (let ((manifest
         (list :schema dream-build-manifest-schema
               :emacs-version emacs-version
               :system-configuration system-configuration
               :native-version (dream-startup-native-version))))
    (should-not (dream-startup-manifest-compatible-p manifest))))

(ert-deftest dream-build-manifest-detects-stale-owned-source ()
  (require 'dream-startup)
  (should-not (dream-startup-manifest-sources-current-p '(:sources nil)))
  (should-not
   (dream-startup-manifest-sources-current-p
    '(:sources (("core/dream-startup.el" . "invalid")))))
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
  (let ((dream-first-input-hook nil)
        (dream-first-file-hook nil)
        (dream-first-buffer-hook nil)
        (dream-init-ui-hook nil)
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
    (should (member "core/dream-hooks.el" files))
    (should (member "core/dream-defaults.el" files))
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
  (let ((file (expand-file-name "early-init.el" user-emacs-directory))
        (value 'missing))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (eq (car-safe form) 'setq)
                (cl-loop for (variable setting) on (cdr form) by #'cddr
                         when (eq variable 'user-lisp-auto-scrape)
                         do (setq value setting)))))
        (end-of-file nil)))
    (should (null value))))

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
        (cl-letf (((symbol-function 'dream-runtime-trampolines-current-p)
                   (lambda () t)))
          (dream-build-write-manifest nil)
          (let ((manifest (dream-startup-read-manifest)))
            (should (dream-startup-manifest-compatible-p manifest))
            (should (eq (plist-get manifest :native) nil))
            (should (assoc "early-init.el" (plist-get manifest :sources)))))
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

(ert-deftest dream-single-drone-build-does-not-refresh-config-manifest ()
  (require 'dream-compile)
  (let (autoloads-called manifest-called)
    (cl-letf (((symbol-function 'dream-build-autoloads)
               (lambda () (setq autoloads-called t)))
              ((symbol-function 'dream-build-write-manifest)
               (lambda (&rest _) (setq manifest-called t))))
      (dream-build-refresh-autoloads))
    (should autoloads-called)
    (should-not manifest-called)))

(ert-deftest dream-build-runtime-artifacts-refreshes-explicit-inputs ()
  (require 'dream-compile)
  (let (environment-called trampolines-called)
    (cl-letf (((symbol-function 'dream-runtime-write-environment-snapshot)
               (lambda () (setq environment-called t)))
              ((symbol-function 'dream-build-trampolines)
               (lambda () (setq trampolines-called t))))
      (dream-build-runtime-artifacts))
    (should environment-called)
    (should trampolines-called)
    (should (equal dream-runtime-required-trampolines
                   '(after-find-file display-startup-echo-area-message
                     describe-buffer-bindings make-process rename-buffer
                     all-completions rename-file)))))

(ert-deftest dream-runtime-prebuilds-directly-advised-primitives ()
  (require 'dream-runtime)
  (dolist (primitive '(after-find-file display-startup-echo-area-message))
    (should (memq primitive dream-runtime-required-trampolines))))

(ert-deftest dream-runtime-supports-emacs-without-native-compilation ()
  (require 'dream-compile)
  (let ((native-comp-enable-subr-trampolines nil)
        compiled)
    (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
              ((symbol-function 'comp-trampoline-compile)
               (lambda (&rest _) (setq compiled t))))
      (setq native-comp-enable-subr-trampolines t)
      (should-not (dream-runtime-trampoline-contract))
      (should (dream-runtime-trampolines-current-p))
      (should-not (dream-build-trampolines))
      (dream-runtime--disable-automatic-compilation)
      (should-not native-comp-enable-subr-trampolines)
      (should-not compiled))))

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

(ert-deftest dream-config-build-removes-source-less-owned-bytecode ()
  (require 'dream-compile)
  (let* ((directory (make-temp-file "dream-stale-bytecode-" t))
         (user-emacs-directory (file-name-as-directory directory))
         (core (expand-file-name "core" directory))
         (orphan (expand-file-name "dream-orphan.elc" core))
         (source (expand-file-name "dream-current.el" core))
         (bytecode (concat source "c")))
    (unwind-protect
        (progn
          (make-directory core t)
          (dolist (file (list orphan source bytecode))
            (with-temp-file file))
          (should (equal (dream-build-clean-stale-bytecode)
                         (list orphan)))
          (should-not (file-exists-p orphan))
          (should (file-exists-p bytecode)))
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
  (require 'dream-core)
  (dolist (type '(dream-hook-error dream-runtime-compilation-error
                  dream-font-error))
    (should (memq 'dream-error (get type 'error-conditions))))
  (should-error (signal 'dream-hook-error '(test)) :type 'dream-error))

(ert-deftest dream-log-honors-inhibit-flag-and-level ()
  (require 'dream-core)
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

(ert-deftest dream-hooks-transient-hook-fires-once-then-disarms ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook nil)
        (dream-test-trigger-a-hook nil)
        (dream-test-trigger-b-hook nil)
        (after-init-time (current-time))
        (calls 0))
    (add-hook 'dream-test-transient-hook (lambda () (cl-incf calls)))
    (dream-run-hook-on 'dream-test-transient-hook
                       '(dream-test-trigger-a-hook
                         dream-test-trigger-b-hook))
    (run-hooks 'dream-test-trigger-a-hook)
    (run-hooks 'dream-test-trigger-a-hook)
    (run-hooks 'dream-test-trigger-b-hook)
    (should (= calls 1))
    (should-not dream-test-transient-hook)
    (should-not dream-test-trigger-a-hook)
    (should-not dream-test-trigger-b-hook))
  ;; Daemon sessions eagerly run every chain on their first frame, even when
  ;; an interactive-session predicate would defer it.
  (let ((dream-test-transient-hook nil)
        (dream-test-trigger-a-hook nil)
        (server-after-make-frame-hook nil)
        (after-init-time (current-time))
        (calls 0))
    (add-hook 'dream-test-transient-hook (lambda () (cl-incf calls)))
    (cl-letf (((symbol-function 'daemonp) (lambda () t)))
      (dream-run-hook-on 'dream-test-transient-hook
                         '(dream-test-trigger-a-hook)
                         (lambda () nil))
      (run-hooks 'server-after-make-frame-hook))
    (should (= calls 1))
    (should-not dream-test-transient-hook)))

(ert-deftest dream-hooks-transient-hook-waits-for-emacs-initialization ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook nil)
        (dream-test-trigger-a-hook nil)
        (after-init-time nil)
        (calls 0))
    (add-hook 'dream-test-transient-hook (lambda () (cl-incf calls)))
    (dream-run-hook-on 'dream-test-transient-hook '(dream-test-trigger-a-hook))
    (run-hooks 'dream-test-trigger-a-hook)
    (should (= calls 0))
    (setq after-init-time (current-time))
    (run-hooks 'dream-test-trigger-a-hook)
    (should (= calls 1))))

(ert-deftest dream-hooks-daemon-dispatch-continues-after-chain-errors ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook nil)
        (dream-test-transient-two-hook nil)
        (dream-test-trigger-a-hook nil)
        (dream-test-trigger-b-hook nil)
        (server-after-make-frame-hook nil)
        (after-init-time (current-time))
        (calls 0))
    (add-hook 'dream-test-transient-hook (lambda () (error "first failed")))
    (add-hook 'dream-test-transient-two-hook (lambda () (cl-incf calls)))
    (cl-letf (((symbol-function 'daemonp) (lambda () t)))
      (dream-run-hook-on 'dream-test-transient-hook
                         '(dream-test-trigger-a-hook))
      (dream-run-hook-on 'dream-test-transient-two-hook
                         '(dream-test-trigger-b-hook))
      (should-error (run-hooks 'server-after-make-frame-hook)
                    :type 'dream-hook-error))
    (should (= calls 1))
    (should-not dream-test-trigger-a-hook)
    (should-not dream-test-trigger-b-hook)))

(ert-deftest dream-run-hooks-continues-across-hook-variables ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook nil)
        (dream-test-transient-two-hook nil)
        (calls 0))
    (add-hook 'dream-test-transient-hook (lambda () (error "first failed")))
    (add-hook 'dream-test-transient-two-hook (lambda () (cl-incf calls)))
    (should-error
     (dream-run-hooks 'dream-test-transient-hook
                      'dream-test-transient-two-hook)
     :type 'dream-hook-error)
    (should (= calls 1))))

(ert-deftest dream-hooks-errors-name-the-hook-and-function-then-disarm ()
  (require 'dream-hooks)
  (let* ((dream-test-transient-hook nil)
         (dream-test-trigger-a-hook nil)
         (after-init-time (current-time))
         (calls nil)
         (before (lambda () (push 'before calls)))
         (boom (lambda () (error "boom")))
         (after (lambda () (push 'after calls))))
    (add-hook 'dream-test-transient-hook before -10)
    (add-hook 'dream-test-transient-hook boom 0)
    (add-hook 'dream-test-transient-hook after 10)
    (dream-run-hook-on 'dream-test-transient-hook '(dream-test-trigger-a-hook))
    (let ((failure (should-error (run-hooks 'dream-test-trigger-a-hook)
                                 :type 'dream-hook-error)))
      (should (eq (plist-get (cdr failure) :hook)
                  'dream-test-transient-hook))
      (let ((failures (plist-get (cdr failure) :failures)))
        (should (= (length failures) 1))
        (should (eq (caar failures) boom))))
    (should (equal (nreverse calls) '(before after)))
    (should-not dream-test-trigger-a-hook)
    ;; A failed attempt must not leave any trigger armed.
    (run-hooks 'dream-test-trigger-a-hook)))

(ert-deftest dream-hooks-disarm-removes-hook-and-advice-triggers ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook '(ignore))
        (dream-test-trigger-a-hook nil)
        (after-init-time (current-time))
        function)
    (unwind-protect
        (progn
          (setq function
                (dream-run-hook-on
                 'dream-test-transient-hook
                 '(dream-test-trigger-a-hook find-file-hook)))
          (should dream-test-trigger-a-hook)
          (should (advice-member-p function 'after-find-file))
          (dream-hooks-disarm 'dream-test-transient-hook)
          (should-not dream-test-trigger-a-hook)
          (should-not (advice-member-p function 'after-find-file)))
      (when function
        (remove-hook 'dream-test-trigger-a-hook function)
        (advice-remove 'after-find-file function)))))

(ert-deftest dream-hooks-advice-trigger-does-not-depend-on-hook-contents ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook nil)
        (find-file-hook nil)
        (after-init-time (current-time))
        (calls 0)
        function)
    (add-hook 'dream-test-transient-hook (lambda () (cl-incf calls)))
    (unwind-protect
        (progn
          (setq function
                (dream-run-hook-on 'dream-test-transient-hook
                                   '(find-file-hook)))
          (funcall function)
          (should (= calls 1))
          (should-not (advice-member-p function 'after-find-file)))
      (when function
        (advice-remove 'after-find-file function)))))

(ert-deftest dream-hooks-first-buffer-uses-window-trigger-frame ()
  (require 'dream-hooks)
  (let* ((dream-test-transient-hook nil)
         (window-buffer-change-functions nil)
         (after-init-time (current-time))
         (frame (selected-frame))
         (window (frame-selected-window frame))
         (original (window-buffer window))
         (target (get-buffer-create "dream-test-real-buffer"))
         (calls 0)
         function)
    (add-hook 'dream-test-transient-hook (lambda () (cl-incf calls)))
    (unwind-protect
        (progn
          (set-window-buffer window target)
          (setq function
                (dream-run-hook-on
                 'dream-test-transient-hook
                 '(window-buffer-change-functions)
                 #'dream-hooks--real-buffer-trigger-p))
          (with-current-buffer (get-buffer-create "*scratch*")
            (funcall function frame))
          (should (= calls 1)))
      (dream-hooks-disarm 'dream-test-transient-hook)
      (set-window-buffer window original)
      (kill-buffer target))))

(ert-deftest dream-defaults-apply-vanilla-baseline ()
  (require 'dream-defaults)
  (should use-short-answers)
  (should (eq ring-bell-function #'ignore))
  (should (eq (default-value 'bidi-paragraph-direction) 'left-to-right))
  (should find-file-visit-truename)
  (should (memq #'window-divider-mode dream-init-ui-hook))
  (should-not (keymap-lookup y-or-n-p-map "SPC")))

(ert-deftest dream-escape-runs-hook-until-success-then-falls-back ()
  (require 'dream-defaults)
  (let ((dream-escape-hook nil)
        (order nil))
    (add-hook 'dream-escape-hook (lambda () (push 'first order) nil))
    (add-hook 'dream-escape-hook (lambda () (push 'second order) t) 50)
    (add-hook 'dream-escape-hook (lambda () (push 'third order) t) 90)
    (dream-escape)
    (should (equal (nreverse order) '(first second))))
  (let ((dream-escape-hook nil))
    (should (eq 'quit (condition-case nil
                          (progn (dream-escape) nil)
                        (quit 'quit))))))

(ert-deftest dream-setq-hook-sets-buffer-local-values ()
  (require 'dream-core)
  (let ((dream-test-mode-hook nil))
    (dream-setq-hook dream-test-mode dream-test-local-var 42)
    (with-temp-buffer
      (run-hooks 'dream-test-mode-hook)
      (should (local-variable-p 'dream-test-local-var))
      (should (= dream-test-local-var 42)))
    (dream-unsetq-hook dream-test-mode dream-test-local-var)
    (should-not dream-test-mode-hook)))

(ert-deftest dream-defadvice-defines-and-attaches-advice ()
  (require 'dream-core)
  (defun dream-test-advised () 'original)
  (unwind-protect
      (progn
        (dream-defadvice dream-test-advice-wrapper (function &rest args)
          "Wrap the return value."
          :around #'dream-test-advised
          (list 'wrapped (apply function args)))
        (should (equal (dream-test-advised) '(wrapped original)))
        (dream-undefadvice dream-test-advice-wrapper (function &rest args)
          :around #'dream-test-advised)
        (should (eq (dream-test-advised) 'original)))
    (advice-remove 'dream-test-advised #'dream-test-advice-wrapper)))

(ert-deftest dream-letf-temporarily-overrides-functions ()
  (require 'dream-core)
  (defun dream-test-letf-target () 'real)
  (should (eq (dream-letf ((defun dream-test-letf-target () 'fake))
                (dream-test-letf-target))
              'fake))
  (should (eq (dream-test-letf-target) 'real)))

(defun dream-test--top-level-definition-symbols (file)
  "Return symbols defined by top-level forms in FILE."
  (let (symbols)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (memq (car-safe form)
                          '(defconst defcustom defmacro defun defvar
                            define-error define-minor-mode))
                (let ((symbol (nth 1 form)))
                  (push (if (eq (car-safe symbol) 'quote)
                            (cadr symbol)
                          symbol)
                        symbols)))))
        (end-of-file nil)))
    symbols))

(ert-deftest dream-core-library-replaces-dream-lib ()
  (should (locate-library "dream-core"))
  (should-not (file-exists-p
               (expand-file-name "core/dream-lib.el" user-emacs-directory))))

(ert-deftest dream-owned-definitions-use-standard-symbol-spelling ()
  (require 'dream-compile)
  (dolist (file (append (dream-build-config-files)
                        (dream-build-tool-files)))
    (dolist (symbol (dream-test--top-level-definition-symbols file))
      (should-not (string-match-p "[!/]" (symbol-name symbol))))))

(ert-deftest dream-runtime-rejects-unapproved-artifact-compilation ()
  (require 'dream-runtime)
  (let ((dream-runtime-compilation-allowed nil)
        (called nil))
    (should-error
     (dream-runtime--guard-compilation
      (lambda () (setq called t)))
     :type 'dream-runtime-compilation-error)
    (should-not called)
    (let ((dream-runtime-compilation-allowed t))
      (dream-runtime--guard-compilation (lambda () (setq called t))))
    (should called)))

(ert-deftest dream-runtime-guard-installation-is-self-healing ()
  (require 'dream-runtime)
  (let ((original-installed dream-runtime--guards-installed)
        (original-advice
         (mapcar
          (lambda (function)
            (cons function
                  (advice-member-p #'dream-runtime--guard-compilation
                                   function)))
          dream-runtime--compiler-functions))
        (source (make-temp-file "dream-runtime-guard-" nil ".el")))
    (unwind-protect
        (progn
          (setq dream-runtime--guards-installed nil)
          (dream-runtime--install-compilation-guards)
          (advice-remove 'byte-compile-file
                         #'dream-runtime--guard-compilation)
          (dream-runtime--install-compilation-guards)
          (should
           (advice-member-p #'dream-runtime--guard-compilation
                            'byte-compile-file))
          (should-error (byte-compile-file source)
                        :type 'dream-runtime-compilation-error)
          (should-not (file-exists-p (concat source "c"))))
      (dolist (entry original-advice)
        (unless (cdr entry)
          (advice-remove (car entry) #'dream-runtime--guard-compilation)))
      (setq dream-runtime--guards-installed original-installed)
      (when (file-exists-p source)
        (delete-file source)))))

(ert-deftest dream-runtime-environment-snapshot-round-trips-as-data ()
  (require 'dream-runtime)
  (let* ((directory (make-temp-file "dream-environment-" t))
         (dream-environment-file
          (expand-file-name "environment.el" directory))
         (pairs `(("PATH" . ,(string-join '("/dream/bin" "/usr/bin")
                                           path-separator))
                  ("LANG" . "en_US.UTF-8"))))
    (unwind-protect
        (cl-letf (((symbol-function 'dream-runtime--collect-environment)
                   (lambda () pairs)))
          (dream-runtime-write-environment-snapshot)
          (should (= (logand (file-modes dream-environment-file) #o777)
                     #o600))
          (let ((snapshot (dream-runtime-read-environment-snapshot))
                (process-environment (copy-sequence process-environment))
                (exec-path nil))
            (should (equal (plist-get snapshot :variables) pairs))
            (should (dream-runtime-apply-environment-snapshot snapshot))
            (should (equal (getenv "LANG") "en_US.UTF-8"))
            (should (equal (car exec-path) "/dream/bin/")))
          (should-not
           (dream-runtime--environment-compatible-p
            (list :schema dream-runtime-environment-schema
                  :system-name (system-name)
                  :system-type system-type
                  :shell (dream-runtime--shell)
                  :variables '(("PATH" . 42)))))
          (should-not
           (dream-runtime--environment-compatible-p
            (list :schema dream-runtime-environment-schema
                  :system-name (system-name)
                  :system-type system-type
                  :shell (dream-runtime--shell)))))
      (delete-directory directory t))))

(ert-deftest dream-incremental-loading-excludes-heavy-idle-features ()
  (require 'init-lsp)
  (require 'init-vc)
  (should-not (member '(:feature lsp-mode) once--incremental-code))
  (should-not (member '(:feature git-commit) once--incremental-code))
  (should-not (member '(:feature with-editor) once--incremental-code))
  (should-not (member '(:feature transient) once--incremental-code)))

(ert-deftest dream-benchmark-rejects-idle-slices-over-budget ()
  (require 'dream-benchmark)
  (let ((summary '(:startup (:p50 0.01 :p95 0.01)
                   :first-input (:p50 0.01 :p95 0.01)
                   :first-file (:p50 0.01 :p95 0.01)
                   :first-prog (:p50 0.01 :p95 0.01)
                   :environment (:p50 0.01 :p95 0.01)
                   :idle-slice-max (:p50 0.049 :p95 0.051)))
        (baseline '(:summary
                    (:startup (:p50 0.01 :p95 0.01)
                     :first-input (:p50 0.01 :p95 0.01)
                     :first-file (:p50 0.01 :p95 0.01)
                     :first-prog (:p50 0.01 :p95 0.01)
                     :environment (:p50 0.01 :p95 0.01)
                     :idle-slice-max (:p50 0.049 :p95 0.06)))))
    (setq baseline
          (append (list :schema dream-benchmark-schema
                        :samples dream-benchmark-samples
                        :emacs-version emacs-version
                        :system-configuration system-configuration
                        :native-version (dream-startup-native-version))
                  baseline))
    (should-error (dream-benchmark--check summary baseline))
    (let* ((directory (make-temp-file "dream-benchmark-budget-" t))
           (dream-benchmark-file (expand-file-name "startup.el" directory)))
      (unwind-protect
          (should-error (dream-benchmark--write-baseline summary))
        (delete-directory directory t)))))

(ert-deftest dream-benchmark-percentiles-use-nearest-rank ()
  (require 'dream-benchmark)
  (should (= (dream-benchmark--percentile (number-sequence 1 15) 0.50)
             8))
  (should (= (dream-benchmark--percentile (number-sequence 1 15) 0.95)
             15)))

(ert-deftest dream-benchmark-child-does-not-preload-dream-libraries ()
  (require 'dream-benchmark)
  (let ((arguments (dream-benchmark--child-arguments)))
    (should-not (member "--load" arguments))
    (should (= (cl-count "--eval" arguments :test #'equal) 1))))

(ert-deftest dream-fonts-reject-unavailable-explicit-families ()
  (require 'dream-fonts)
  (let ((dream-font "Dream Missing Font")
        (frame (selected-frame)))
    (dream-fonts--clear-state)
    (cl-letf (((symbol-function 'display-multi-font-p) (lambda (&optional _) t))
              ((symbol-function 'frame-initial-p) (lambda (&optional _) nil))
              ((symbol-function 'font-family-list)
               (lambda (&optional _) '("Menlo"))))
      (should-error (dream-fonts-apply frame) :type 'dream-font-error))))

(ert-deftest dream-fonts-cache-frame-and-display-work ()
  (require 'dream-fonts)
  (let ((dream-font nil)
        (dream-variable-pitch-font nil)
        (dream-serif-font nil)
        (dream-symbol-font nil)
        (dream-emoji-font nil)
        (dream-cjk-font nil)
        (frame (selected-frame))
        (family-queries 0)
        (face-calls 0)
        fontset-calls)
    (dream-fonts--clear-state)
    (cl-letf (((symbol-function 'display-multi-font-p) (lambda (&optional _) t))
              ((symbol-function 'frame-initial-p) (lambda (&optional _) nil))
              ((symbol-function 'font-family-list)
               (lambda (&optional _)
                 (cl-incf family-queries)
                 '("Menlo" "Apple Symbols" "Apple Color Emoji"
                   "PingFang SC" "Symbols Nerd Font Mono")))
              ((symbol-function 'set-face-attribute)
               (lambda (&rest _) (cl-incf face-calls)))
              ((symbol-function 'set-fontset-font)
               (lambda (&rest args) (push args fontset-calls))))
      (should (dream-fonts-apply frame))
      (should-not (dream-fonts-apply frame))
      (should (= family-queries 1))
      (should (= face-calls 4))
      (should (= (length fontset-calls) 7))
      (dolist (call fontset-calls)
        (should-not (car call))
        (should (eq (nth 3 call) frame)))
      (should-not (cl-find 'prepend fontset-calls :key (lambda (call)
                                                         (nth 4 call)))))))

(ert-deftest dream-fonts-cover-frames-created-outside-server-mode ()
  (require 'dream-fonts)
  (should (memq #'dream-fonts-apply after-make-frame-functions)))

(ert-deftest dream-font-reload-clears-caches-and-replays-frames ()
  (require 'dream-fonts)
  (let ((dream-font nil)
        (dream-variable-pitch-font nil)
        (dream-serif-font nil)
        (dream-symbol-font nil)
        (dream-emoji-font nil)
        (dream-cjk-font nil)
        (face-font-rescale-alist nil)
        (frame (selected-frame))
        (cache-clears 0)
        (family-queries 0)
        (face-calls 0)
        (fontset-calls 0))
    (dream-fonts--clear-state)
    (unwind-protect
        (cl-letf (((symbol-function 'display-multi-font-p)
                   (lambda (&optional _) t))
                  ((symbol-function 'frame-initial-p)
                   (lambda (&optional _) nil))
                  ((symbol-function 'font-family-list)
                   (lambda (&optional _)
                     (cl-incf family-queries)
                     '("Menlo" "Apple Symbols" "Apple Color Emoji"
                       "PingFang SC" "Symbols Nerd Font Mono")))
                  ((symbol-function 'set-face-attribute)
                   (lambda (&rest _) (cl-incf face-calls)))
                  ((symbol-function 'set-fontset-font)
                   (lambda (&rest _) (cl-incf fontset-calls)))
                  ((symbol-function 'clear-font-cache)
                   (lambda () (cl-incf cache-clears)))
                  ((symbol-function 'frame-list) (lambda () (list frame))))
          (dream-fonts-apply frame)
          (should (dream-font-reload))
          (should (= cache-clears 1))
          (should (= family-queries 2))
          (should (= face-calls 8))
          (should (= fontset-calls 14)))
      (dream-fonts--clear-state))))

(ert-deftest dream-font-commands-resize-reload-and-reset ()
  (require 'dream-fonts)
  (let ((dream-font-size 11.0)
        (dream-font-size-step 1.5)
        (dream-fonts--initial-size nil)
        (reloads 0))
    (cl-letf (((symbol-function 'display-multi-font-p) (lambda (&optional _) t))
              ((symbol-function 'dream-font-reload)
               (lambda () (cl-incf reloads))))
      (dream-font-increase 2)
      (should (= dream-font-size 14.0))
      (dream-font-decrease 1)
      (should (= dream-font-size 12.5))
      (dream-font-reset)
      (should (= dream-font-size 11.0))
      (should (= reloads 3)))))

(ert-deftest dream-font-resize-rolls-back-after-reload-errors ()
  (require 'dream-fonts)
  (let ((dream-font-size 11.0)
        (dream-font-size-step 1.0)
        (dream-fonts--initial-size nil)
        (reloads 0))
    (cl-letf (((symbol-function 'display-multi-font-p) (lambda (&optional _) t))
              ((symbol-function 'dream-font-reload)
               (lambda ()
                 (cl-incf reloads)
                 (when (= reloads 1)
                   (error "font reload failed")))))
      (should-error (dream-font-increase 1))
      (should (= dream-font-size 11.0))
      (should-not dream-fonts--initial-size)
      (should (= reloads 2)))))

(ert-deftest dream-fonts-are-a-terminal-no-op ()
  (require 'dream-fonts)
  (let ((dream-font-size 11.0))
    (cl-letf (((symbol-function 'display-multi-font-p) (lambda (&optional _) nil)))
      (should-not (dream-fonts-apply))
      (should-not (dream-font-increase 1))
      (should (= dream-font-size 11.0)))))

(ert-deftest dream-defaults-redirect-state-files-into-local ()
  (require 'dream-defaults)
  (dolist (value (list custom-file project-list-file nsm-settings-file
                       url-configuration-directory multisession-directory))
    (should (stringp value))
    (should (string-prefix-p dream-local-directory
                             (expand-file-name value)))))

(provide 'dream-core-test)
;;; dream-core-test.el ends here.
