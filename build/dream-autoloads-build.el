;;; dream-autoloads-build.el --- Build Dream autoload cache. -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-eval-when (compile)
  (require 'borg))
;;
;;; Commentary:
;;
;; Overview / 概览
;; ---------------
;; Dream autoloads is the thin layer between Borg's per-package autoload
;; generation and Dream's startup path.  Borg still owns package discovery,
;; build order, submodule activation, byte/native compilation, and each
;; package's own DRONE-autoloads.el or DRONE-loaddefs.el file.  This library
;; only combines those already-generated files into one source cache that
;; startup can load without requiring Borg.
;;
;; Dream autoloads 是 Borg 的“逐包 autoload 生成结果”和 Dream 启动路径之间的
;; 薄封装。包发现、构建顺序、子模块启用状态、byte/native 编译，以及每个包
;; 自己的 DRONE-autoloads.el 或 DRONE-loaddefs.el 仍然由 Borg 负责。本文件
;; 只把 Borg 已经生成好的 autoload 文件聚合成一个源码缓存，让启动阶段不必
;; require Borg。
;;
;; Build-time flow / 构建阶段流程
;; ------------------------------
;; 1. Borg's Makefile starts Emacs in batch mode.  The top-level Makefile loads
;;    this file before loading Borg.
;;    Borg 的 Makefile 以 batch 方式启动 Emacs；顶层 Makefile 会在加载 Borg
;;    之前加载本文件。
;; 2. In noninteractive sessions this file registers a `with-eval-after-load'
;;    hook for Borg.
;;    在 noninteractive 会话中，本文件通过 `with-eval-after-load' 等 Borg
;;    加载完成后再安装 advice。
;; 3. `dream-autoloads-install-borg-advices' captures the pre-Borg values of
;;    startup-sensitive variables, then advises Borg build commands.
;;    `dream-autoloads-install-borg-advices' 会先捕获 Borg 初始化前的启动敏感
;;    变量基线，再 advice Borg 的构建命令。
;; 4. Borg runs its normal build.  Each drone still owns its generated
;;    DRONE-autoloads.el or DRONE-loaddefs.el file.
;;    Borg 照常构建；每个 drone 仍然维护自己的 autoload 生成文件。
;; 5. After a successful batch rebuild, or after an individual Borg build,
;;    `dream-autoloads-generate' writes `.local/cache/borg/autoloads.el'
;;    and `.local/cache/borg/autoloads-state.el'.
;;    全量重建或单包构建成功后，`dream-autoloads-generate' 写入聚合缓存和状态
;;    文件。
;;
;; Startup flow / 启动阶段流程
;; ---------------------------
;; 1. `init.el' adds `core/' directly, then uses `dream-paths-add-load-paths'
;;    for the `lib/' and `lisp/' trees before requiring startup libraries.
;;    `init.el' 直接加入 `core/'，再由 `dream-paths-add-load-paths' 配置
;;    `lib/' 和 `lisp/' 目录树，之后 require 启动库。
;; 2. `dream-autoloads-initialize' performs a cheap cache-format check and then
;;    loads only `dream-autoloads-file'.  The fast path does not require Borg.
;;    `dream-autoloads-initialize' 先做低成本的缓存格式检查，然后只加载
;;    `dream-autoloads-file'；正常快速路径不会 require Borg。
;; 3. The generated cache appends Borg package paths and installs package
;;    autoloads.  It must not overwrite Dream's own startup `load-path'.
;;    生成缓存只追加 Borg 包路径并安装包 autoload，不应该覆盖 Dream 启动时
;;    已经建立的 `load-path'。
;; 4. Staleness is checked after startup on an idle timer.  A stale cache only
;;    produces a warning; it is never regenerated during startup.
;;    过期检查在启动后通过 idle timer 执行；过期只提醒，不在启动时重建。
;; 5. Stale or format-mismatched state is only warned about after startup.  Only
;;    missing or unreadable cache source falls back to `borg-initialize'.
;;    状态过期或格式版本不一致只在启动后提醒。只有缓存源码缺失或不可读时才
;;    fallback 到 `borg-initialize'。
;;
;; Design contracts / 设计约束
;; ---------------------------
;; - The generated source is byte-compiled, but never native-compiled.
;;   生成源码会 byte-compile，但不会 native-compile。
;; - This library no longer scans Dream's local `core/' or `lisp/' autoload
;;   cookies.  A future local-autoload directory can be added as a separate
;;   layer.
;;   本库不再扫描 Dream 本地 `core/' 或 `lisp/' 的 autoload cookie；未来的个人
;;   autoload 目录应作为单独层加入。
;; - Startup may warn about staleness, but it must not regenerate the cache.
;;   启动时可以提醒缓存过期，但不允许重建缓存。
;; - Cached variables are replayed incrementally when a build-time baseline is
;;   available.  This avoids dumping Emacs' full default alists into startup.
;;   有构建期基线时，cached variables 以增量形式 replay，避免把 Emacs 默认的
;;   大型 alist 整表写入启动缓存。
;; - Package autoload files are parsed at build time, normalized, and flattened
;;   into the generated cache.  The startup cache should not preserve a
;;   per-package `load-file-name' wrapper unless a form has been explicitly
;;   rewritten to a constant source path.
;;   包 autoload 文件会在构建期被解析、清理并拍平成一个生成缓存。启动缓存不应
;;   保留“每个包一个 `load-file-name' wrapper”的形状；确实依赖源文件名的表单
;;   会在生成时被改写成常量路径。
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dream-paths)

(defgroup dream-autoloads nil
  "Single-file autoload cache for Dream Emacs.
Dream Emacs 的单文件 autoload 聚合缓存。"
  :group 'dream)

(defcustom dream-autoloads-cached-vars
  '(custom-theme-load-path
    auto-mode-alist
    interpreter-mode-alist
    magic-mode-alist
    magic-fallback-mode-alist
    Info-directory-list)
  "Variables replayed by `dream-autoloads-file'.
由聚合缓存 replay 的启动敏感变量。生成时优先写入 Borg 带来的增量，而
不是覆盖 Emacs 已有的完整变量值。"
  :type '(repeat symbol))

(defcustom dream-autoloads-path-cached-vars
  '(custom-theme-load-path Info-directory-list)
  "Cached variables whose string members should be abbreviated as file names.
这些 cached vars 中的字符串成员会按文件名缩写，减少绝对路径差异。"
  :type '(repeat symbol))

(defcustom dream-autoloads-strip-docstrings t
  "Whether generated package autoloads should omit copied docstrings.
Autoload docstrings are useful for help buffers before a package is loaded, but
they also make the startup cache much larger.  When this is non-nil, the cache
keeps the lazy loading behavior and drops those copied strings.

是否从生成缓存里剥离包 autoload 复制来的 docstring。开启后仍保留延迟
加载行为，但包真正加载前的 help 文档会更少；好处是缓存更小，启动解析更快。"
  :type 'boolean)

(defconst dream-autoloads--state-version 6
  "Internal cache format version for Dream autoload state.
Dream autoload 状态文件的内部缓存格式版本。")
(defconst dream-autoloads--binding-vars
  '(load-path
    custom-theme-load-path
    auto-mode-alist
    interpreter-mode-alist
    magic-mode-alist
    magic-fallback-mode-alist
    Info-directory-list)
  "Variables dynamically rebound while computing generated autoload effects.
生成阶段动态绑定的变量集合，用来隔离 Borg/package autoload 对启动敏感变量
造成的影响。")
(defvar dream-autoloads--cached-vars-baseline nil
  "Captured pre-Borg values for variables in `dream-autoloads--binding-vars'.
`dream-autoloads--binding-vars' 中各变量在 Borg 初始化前的基线值。")
(defvar dream-autoloads--defer-generate nil
  "Non-nil means defer per-drone cache generation during batch rebuilds.
非 nil 表示全量 batch rebuild 期间延迟单个 drone 的缓存生成。")

(define-error 'dream-autoload-error
  "Dream autoload error / Dream autoload 错误")

(defun dream-autoloads--unquote (exp)
  "Return EXP without leading `quote' or `function' wrappers.
This normalizes expressions such as `foo', \\='foo, and #\\='foo before comparing
them to cached variable names.

去掉 EXP 外层的 `quote' 或 `function'，方便比较 `add-to-list' 这类表单
实际操作的变量名。"
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun dream-autoloads--relative-path (file)
  "Return FILE relative to `user-emacs-directory' when possible.
State files use relative paths for files inside this configuration so the cache
is less sensitive to absolute MSYS2/Windows path spelling.

尽量把 FILE 转成相对 `user-emacs-directory' 的路径，降低 MSYS2/Windows
绝对路径写法差异对状态文件的影响。"
  (let ((file (expand-file-name file))
        (root (file-name-as-directory (expand-file-name user-emacs-directory))))
    (if (file-in-directory-p file root)
        (file-relative-name file root)
      (abbreviate-file-name file))))

(defun dream-autoloads--expand-state-path (file)
  "Expand FILE from an entry in `dream-autoloads-state-file'.
Relative paths are resolved from `user-emacs-directory'; home-relative paths are
resolved through `expand-file-name'.

把状态文件中记录的路径展开为可访问路径；相对路径以
`user-emacs-directory' 为根。"
  (cond
   ((file-name-absolute-p file) file)
   ((string-prefix-p "~" file) (expand-file-name file))
   (t (expand-file-name file user-emacs-directory))))

(defun dream-autoloads--dedupe-paths (paths)
  "Return PATHS without duplicates, preserving order.
Paths are compared after expansion and directory-name normalization, but the
original spelling of the first occurrence is preserved.

按展开和目录规范化后的路径去重，同时保留第一次出现时的原始写法和顺序。"
  (let (seen result)
    (dolist (path paths (nreverse result))
      (let ((key (directory-file-name (expand-file-name path))))
        (unless (member key seen)
          (push key seen)
          (push path result))))))

(defun dream-autoloads--path-entry (file)
  "Return a state entry for FILE.
The entry records relative path, modification time, and size; nil means FILE no
longer exists and should not be written into the state file.

为 FILE 生成状态条目，记录相对路径、修改时间和大小；文件不存在则返回
nil，不写入状态文件。"
  (when-let* ((attrs (file-attributes file)))
    (list (dream-autoloads--relative-path file)
          (file-attribute-modification-time attrs)
          (nth 7 attrs))))

(defun dream-autoloads--dir-entry (dir)
  "Return a state entry for DIR.
Directory entries record only path and modification time; they catch Borg
load-path directory changes relevant to the generated package cache.

为目录生成状态条目，只记录路径和修改时间，用于捕获 Borg load-path 目录
中新增/删除 autoload 输入的情况。"
  (when-let* ((attrs (file-attributes dir)))
    (list (dream-autoloads--relative-path (directory-file-name dir))
          (file-attribute-modification-time attrs))))

(defun dream-autoloads--entry-current-p (entry &optional directory-p)
  "Return non-nil if ENTRY still matches the file system.
When DIRECTORY-P is non-nil, compare only directory modification data; otherwise
also compare file size.

检查状态条目 ENTRY 是否仍与文件系统一致；目录只比较修改时间，普通文件
还会比较大小。"
  (let* ((file (dream-autoloads--expand-state-path (car entry)))
         (attrs (file-attributes file)))
    (and attrs
         (equal (cadr entry) (file-attribute-modification-time attrs))
         (or directory-p
             (equal (nth 2 entry) (nth 7 attrs))))))

(defun dream-autoloads--borg-drone-active-p (drone)
  "Return non-nil if Borg DRONE should contribute to the cache.
Missing and disabled drones are skipped, matching Borg's own activation model.

判断 Borg drone 是否应该进入聚合缓存；缺失或禁用的 drone 会跳过，以匹配
Borg 自己的激活模型。"
  (and (file-exists-p (borg-worktree drone))
       (not (and (fboundp 'borg--drone-disabled-p)
                 (borg--drone-disabled-p drone)))))

(defun dream-autoloads--borg-drones ()
  "Return Borg drones in Borg's configured order.
The function reads Borg's `.gitmodules' view instead of inferring drones from
`load-path'.

按 Borg 配置顺序返回 drone 列表；这里读取 Borg 对 `.gitmodules' 的视图，
而不是从 `load-path' 反推。"
  (require 'borg)
  (mapcar #'car (borg-drones 'raw)))

(defun dream-autoloads--borg-git-state-files (drone)
  "Return Git files that record DRONE's checked-out revision.
The state checker watches these files so changing a submodule commit makes the
single autoload cache stale, even if the generated package autoload file has not
yet been rebuilt.

返回记录 DRONE 当前检出版本的 Git 文件。这样即使包的 autoload 文件尚未
重建，子模块 commit 变化也会让聚合缓存被标记为过期。"
  (let* ((gitdir (ignore-errors (borg-gitdir drone)))
         (head (and gitdir (expand-file-name "HEAD" gitdir)))
         files)
    (when (and head (file-readable-p head))
      (push head files)
      (let ((ref (with-temp-buffer
                   (insert-file-contents head)
                   (string-trim (buffer-string)))))
        (if (string-match "\\`ref: \\(.+\\)\\'" ref)
            (let ((ref-file (expand-file-name (match-string 1 ref) gitdir))
                  (packed-refs (expand-file-name "packed-refs" gitdir)))
              (if (file-readable-p ref-file)
                  (push ref-file files)
                (when (file-readable-p packed-refs)
                  (push packed-refs files)))))))
    files))

(defun dream-autoloads--borg-paths ()
  "Return Borg load paths, info paths, generated autoload files, and Git state.
The returned plist is the build-time source of truth for what will be cached.

收集 Borg 的 load-path、info 路径、已生成的包 autoload 文件和 Git 状态
文件；返回的 plist 是构建聚合缓存的输入来源。"
  (require 'borg)
  (let (autoload-files git-files load-paths info-paths)
    (dolist (drone (dream-autoloads--borg-drones))
      (when (dream-autoloads--borg-drone-active-p drone)
        (setq git-files
              (nconc (dream-autoloads--borg-git-state-files drone)
                     git-files))
        (dolist (dir (borg-load-path drone))
          (when (file-directory-p dir)
            (push dir load-paths)
            (let ((autoloads (expand-file-name
                              (format "%s-autoloads.el" drone) dir))
                  (loaddefs (expand-file-name
                             (format "%s-loaddefs.el" drone) dir)))
              (cond
               ((file-readable-p autoloads)
                (push autoloads autoload-files))
               ((file-readable-p loaddefs)
                (push loaddefs autoload-files))))))
        (dolist (dir (borg-info-path drone))
          (when (file-directory-p dir)
            (push dir info-paths)))))
    (list :autoload-files (dream-autoloads--dedupe-paths
                           (nreverse autoload-files))
          :git-files (dream-autoloads--dedupe-paths (nreverse git-files))
          :load-paths (dream-autoloads--dedupe-paths (nreverse load-paths))
          :info-paths (dream-autoloads--dedupe-paths (nreverse info-paths)))))

(defun dream-autoloads--abbreviate-path-list (value)
  "Abbreviate string members in path list VALUE.
Non-string members such as t in `custom-theme-load-path' are preserved.

缩写路径列表 VALUE 中的字符串成员；例如 `custom-theme-load-path' 里的 t
会原样保留。"
  (mapcar (lambda (item)
            (if (stringp item)
                (abbreviate-file-name item)
              item))
          value))

(defun dream-autoloads--current-cached-values ()
  "Return cached variable values from the current dynamic environment.
Path-like variables are abbreviated before being printed into the generated
autoload cache.

读取当前动态环境中的 cached var 值；路径类变量会在写入生成缓存前做路径
缩写。"
  (mapcar (lambda (var)
            (cons var
                  (when (boundp var)
                    (let ((value (copy-tree (symbol-value var))))
                      (if (memq var dream-autoloads-path-cached-vars)
                          (dream-autoloads--abbreviate-path-list value)
                        value)))))
          dream-autoloads-cached-vars))

(defun dream-autoloads--baseline-value (var)
  "Return the captured baseline value for VAR.
Path-like values are abbreviated the same way generated cache values are, so
delta computation compares equivalent path spellings.

返回 VAR 在 Borg 初始化前捕获到的基线值；路径类值会按生成缓存同样的方式
缩写，保证增量比较使用等价路径写法。"
  (when-let* ((entry (assq var dream-autoloads--cached-vars-baseline)))
    (let ((value (copy-tree (cdr entry))))
      (if (memq var dream-autoloads-path-cached-vars)
          (dream-autoloads--abbreviate-path-list value)
        value))))

(defun dream-autoloads--list-delta (value baseline)
  "Return list members in VALUE that are not present in BASELINE.
The original order from VALUE is preserved.

返回 VALUE 中存在但 BASELINE 中不存在的列表成员，并保留 VALUE 中的顺序。"
  (cl-loop for item in value
           unless (member item baseline)
           collect item))

(defun dream-autoloads-capture-baseline ()
  "Remember cached variable values before Borg activates drones.
During `make build' this is called before `borg-initialize' mutates `load-path'
and related variables; generation later replays package autoloads in a clean
dynamic binding based on this snapshot.

在 Borg 激活 drone 前捕获 cached vars 的基线。之后生成缓存时会在这个基线
上重放包 autoload 的影响，从而只输出 Borg 带来的增量。"
  (setq dream-autoloads--cached-vars-baseline
        (mapcar (lambda (var)
                  (cons var (and (boundp var)
                                 (copy-tree (symbol-value var)))))
                dream-autoloads--binding-vars)))

(defun dream-autoloads--collect-cached-values (autoload-files load-paths info-paths)
  "Return cached values after applying AUTOLOAD-FILES in a clean binding.
LOAD-PATHS and INFO-PATHS are Borg paths that have no guaranteed autoload file.
This computes the exact variable values startup should restore from the single
cache without requiring Borg.

在干净的动态绑定中加载 AUTOLOAD-FILES，并叠加 Borg 的 LOAD-PATHS 与
INFO-PATHS，得到启动缓存需要 replay 的变量结果。"
  (if (not dream-autoloads--cached-vars-baseline)
      (dream-autoloads--current-cached-values)
    (cl-progv (mapcar #'car dream-autoloads--cached-vars-baseline)
        (mapcar (lambda (entry) (copy-tree (cdr entry)))
                dream-autoloads--cached-vars-baseline)
      (dolist (dir (reverse load-paths))
        (add-to-list 'load-path dir))
      (defvar Info-directory-list nil)
      (dolist (dir info-paths)
        (push dir Info-directory-list))
      (dolist (file autoload-files)
        (with-demoted-errors "Error loading autoloads while caching vars: %s"
          (load file nil t t)))
      (dream-autoloads--current-cached-values))))

(defun dream-autoloads--cached-vars-forms (values)
  "Return incremental forms for cached variable VALUES.
The generated cache should not overwrite Emacs' existing startup variables.
When a build-time baseline is available, only Borg's additions are replayed.
Without a baseline, fall back to setting the whole value so manual generation is
still correct.

把 cached var 的最终值转换成生成缓存中的增量 replay 表达式。有构建期基线
时只输出 Borg 增加的部分；没有基线时才退回到整值 `setq'，保证手工调用仍可用。"
  (let (forms)
    (dolist (entry values)
      (let* ((var (car entry))
             (value (cdr entry))
             (baseline (dream-autoloads--baseline-value var))
             (delta (and (listp value)
                         (listp baseline)
                         (dream-autoloads--list-delta value baseline))))
        (cond
         (delta
          (push `(progn
                   (defvar ,var nil)
                   (dolist (entry ',(reverse delta))
                     (add-to-list ',var entry)))
                forms))
         ((and (not dream-autoloads--cached-vars-baseline)
               (not (equal value baseline)))
          (push `(setq ,var ',value) forms)))))
    (nreverse forms)))

(defun dream-autoloads--load-path-form (load-paths)
  "Return a form that adds Borg LOAD-PATHS without overwriting `load-path'.
返回只追加 Borg LOAD-PATHS 的表达式，避免覆盖 `init.el' 已设置的本地
`load-path'。"
  `(dolist (dir ',(reverse (dream-autoloads--abbreviate-path-list load-paths)))
     (add-to-list 'load-path dir)))

(defun dream-autoloads--trim-trailing-nils (form min-length)
  "Return FORM without nil tail elements beyond MIN-LENGTH.
This keeps generated forms smaller after docstrings are stripped.

剥离 docstring 后，删除 FORM 尾部多余的 nil，但至少保留 MIN-LENGTH 个元素。"
  (let ((form (copy-sequence form)))
    (while (and (> (length form) min-length)
                (null (car (last form))))
      (setq form (butlast form)))
    form))

(defun dream-autoloads--strip-docstring (form)
  "Return FORM with copied package docstrings removed when appropriate.
Only generated metadata forms are simplified; executable package setup forms are
left untouched.

在安全的元数据表单中移除包 autoload 复制来的 docstring；真正执行初始化逻辑
的表单保持原样。"
  (if (not dream-autoloads-strip-docstrings)
      form
    (pcase (car-safe form)
      ('autoload
       (let ((form (copy-sequence form)))
         (when (nthcdr 3 form)
           (setcar (nthcdr 3 form) nil))
         (dream-autoloads--trim-trailing-nils form 3)))
      ((or 'defvar 'defconst)
       (if (nthcdr 3 form)
           (let ((form (copy-sequence form)))
             (setcdr (nthcdr 2 form) nil)
             form)
         form))
      (_ form))))

(defun dream-autoloads--substitute-file-context (form source)
  "Return FORM with load-file context symbols replaced by SOURCE.
Generated Borg autoload files commonly use `load-file-name' only to add their
own directory to `load-path'.  Those forms are removed elsewhere.  For the rare
remaining form that keeps `load-file-name' as a real value, flattening the
generated cache must preserve the value without a per-package wrapper.

把 FORM 中自由出现的 `load-file-name' 和 `load-true-file-name' 改写成
SOURCE 常量。这样聚合缓存可以被拍平，而不用保留“正在加载某个包 autoload 文件”
的运行时 wrapper。"
  (let ((source (abbreviate-file-name source)))
    (cl-labels
        ((walk (node)
           (cond
            ((eq node 'load-file-name) source)
            ((eq node 'load-true-file-name) source)
            ((not (consp node)) node)
            ((eq (car node) 'quote) node)
            (t (cons (walk (car node))
                     (walk (cdr node)))))))
      (walk form))))

(defun dream-autoloads--cleanup-form (form &optional drop-cached-vars source)
  "Return cleaned FORM, or nil when it should be discarded.
When DROP-CACHED-VARS is non-nil, remove forms that mutate cached variables.
This avoids duplicating package `provide' forms, definition-prefix registration,
and `add-to-list' updates already represented by the generated cache prologue.
SOURCE is the generated package autoload file the form came from; if provided,
file-context symbols are rewritten so the final cache can stay flat.

清理包 autoload 表达式。可以丢弃重复的 `provide'、前缀注册以及已由生成
缓存序言统一 replay 的 cached var 更新。SOURCE 是该表单来源的包 autoload 文件；
提供后会把文件上下文符号改写成常量，以便最终缓存保持扁平结构。"
  (let ((fn (car-safe form)))
    (cond
     ((memq fn '(provide custom-autoload register-definition-prefixes))
      nil)
     ((and drop-cached-vars
           (eq fn 'add-to-list)
           (memq (dream-autoloads--unquote (cadr form))
                 dream-autoloads--binding-vars))
      nil)
     (t (let ((form (dream-autoloads--strip-docstring form)))
          (if source
              (dream-autoloads--substitute-file-context form source)
            form))))))

(defun dream-autoloads--sanitize-buffer-for-read (source)
  "Prepare the current buffer, generated from SOURCE, for `read'.
Generated package autoloads sometimes contain #$ placeholders; replacing them
with the source file keeps literal reads safe outside package.el's context.  The
replacement deliberately skips strings, following Doom's autoload scanner.

为 `read' 预处理当前 buffer。包 autoload 中可能含有 #$ 占位符，这里替换为
源文件名，避免脱离 package.el 上下文时读取失败。替换时会跳过字符串内部，这一点
跟 Doom 的 autoload scanner 一致。"
  (goto-char (point-min))
  (with-syntax-table emacs-lisp-mode-syntax-table
    (while (search-forward "#$" nil t)
      (unless (save-excursion
                (nth 3 (syntax-ppss (match-beginning 0))))
        (replace-match (prin1-to-string (abbreviate-file-name source)) t t)))))

(defun dream-autoloads--forms-from-buffer (source &optional drop-cached-vars)
  "Return cleaned autoload forms from current buffer.
SOURCE is the package autoload file being parsed.  When DROP-CACHED-VARS is
non-nil, cached-variable mutations are discarded.  Forms are returned as a flat
list; references to `load-file-name' are rewritten to SOURCE instead of being
preserved through a per-file wrapper.

从当前 buffer 读取并清理 autoload 表达式。返回值是扁平表单列表；若表单依赖
`load-file-name'，会在构建期改写成 SOURCE 常量，而不是保留每个文件一个 wrapper。"
  (dream-autoloads--sanitize-buffer-for-read source)
  (goto-char (point-min))
  (let (forms)
    (condition-case err
        (while t
          (when-let* ((form (dream-autoloads--cleanup-form
                             (read (current-buffer))
                             drop-cached-vars
                             source)))
            (push form forms)))
      (end-of-file)
      (error
       (signal 'dream-autoload-error (list source err))))
    (nreverse forms)))

(defun dream-autoloads--scan-literal-file (file)
  "Return a flat list of cleaned forms from package autoload FILE.
This path is used for Borg-generated DRONE-autoloads.el and DRONE-loaddefs.el
files.

读取 Borg 已生成的包 autoload 文件，并返回清理后的扁平表达式列表。"
  (with-temp-buffer
    (insert-file-contents file)
    (dream-autoloads--forms-from-buffer file t)))

(defun dream-autoloads--autoload-forms (autoload-files)
  "Return all autoload forms from AUTOLOAD-FILES.
Package autoload files are read literally; local configuration autoloads are not
scanned here.

从 AUTOLOAD-FILES 聚合所有包 autoload 表达式；这里不扫描本地配置目录。"
  (let (forms)
    (dolist (file autoload-files)
      (setq forms (append forms (dream-autoloads--scan-literal-file file))))
    forms))

(defun dream-autoloads--build-state (autoload-files load-paths &optional extra-files)
  "Return state describing AUTOLOAD-FILES, LOAD-PATHS, and EXTRA-FILES.
The state is intentionally data-only so startup can check freshness without
loading Borg or regenerating autoloads.

构造描述 AUTOLOAD-FILES、LOAD-PATHS 和 EXTRA-FILES 的状态数据。状态文件
只包含数据，启动时可只读判断缓存是否过期，而不需要加载 Borg 或重建缓存。"
  (let* ((files (dream-autoloads--dedupe-paths
                 (append (list (expand-file-name ".gitmodules"
                                                 user-emacs-directory))
                         autoload-files
                         extra-files)))
         (dirs (dream-autoloads--dedupe-paths load-paths)))
    (list :version dream-autoloads--state-version
          :generated-at (current-time)
          :files (sort (delq nil (mapcar #'dream-autoloads--path-entry files))
                       (lambda (a b) (string< (car a) (car b))))
          :directories (sort (delq nil (mapcar #'dream-autoloads--dir-entry dirs))
                             (lambda (a b) (string< (car a) (car b)))))))

(defun dream-autoloads--write-forms (file forms)
  "Write FORMS to autoload cache FILE.
The output is deterministic Lisp source intended for byte compilation and is
marked to avoid native compilation.

把 FORMS 写入聚合 autoload 缓存 FILE。输出是启动时直接加载的确定性源码，
并显式标记为不进行 byte/native 编译。"
  (make-directory (file-name-directory file) 'parents)
  (condition-case-unless-debug err
      (with-temp-file file
        (setq-local coding-system-for-write 'utf-8)
        (let ((standard-output (current-buffer))
              (print-quoted t)
              (print-level nil)
              (print-length nil))
          (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                  ";; This file is generated by Dream autoloads.  Do not edit.\n\n")
          (dolist (form forms)
            (prin1 form)
            (insert "\n\n"))
          (insert ";; Local Variables:"
                  "\n;; version-control: never"
                  "\n;; no-update-autoloads: t"
                  "\n;; no-native-compile: t"
                  "\n;; End:\n")
          t))
    (error
     (ignore-errors (delete-file file))
     (signal 'dream-autoload-error (list file err)))))

(defun dream-autoloads--write-state (state)
  "Write autoload STATE to `dream-autoloads-state-file'.
STATE is consumed by staleness checks.

把 STATE 写入状态文件；过期检查会读取它。"
  (make-directory (file-name-directory dream-autoloads-state-file) 'parents)
  (with-temp-file dream-autoloads-state-file
    (setq-local coding-system-for-write 'utf-8)
    (prin1 state (current-buffer))
    (insert "\n")))

(defun dream-autoloads--cleanup-obsolete (&optional keep-file)
  "Delete obsolete autoload cache files, keeping KEEP-FILE.
This removes timestamped cache files left by the old implementation while
preserving the current stable cache name.

删除旧实现留下的时间戳缓存文件，同时保留当前稳定文件名对应的缓存。"
  (when (file-directory-p dream-autoloads-directory)
    (dolist (file (directory-files
                   dream-autoloads-directory t
                   "\\`autoload-[0-9]+-[0-9]+\\.el\\'"))
      (unless (and keep-file (file-equal-p file keep-file))
        (ignore-errors (delete-file file))))))

(defun dream-autoloads-generate (&optional file)
  "Generate Dream's single autoload cache into FILE.
This is the build-time entry point used by Borg advices and by `make autoloads'.
It writes the cache, writes the state file, and removes old timestamped caches.

生成 Dream 的聚合 autoload 缓存。该函数是 Borg advice 和 `make autoloads'
使用的构建期入口，会写入缓存、写入状态文件，并清理旧的时间戳缓存。"
  (interactive)
  (require 'borg)
  (let* ((file (or file dream-autoloads-file))
         (paths (dream-autoloads--borg-paths))
         (autoload-files (plist-get paths :autoload-files))
         (git-files (plist-get paths :git-files))
         (load-paths (plist-get paths :load-paths))
         (info-paths (plist-get paths :info-paths))
         (cached-values (dream-autoloads--collect-cached-values
                         autoload-files load-paths info-paths))
         (forms (append (list (dream-autoloads--load-path-form load-paths))
                        (dream-autoloads--cached-vars-forms cached-values)
                        (dream-autoloads--autoload-forms autoload-files)))
         (state (dream-autoloads--build-state
                 autoload-files load-paths git-files)))
    (message "Generating Dream autoload cache...")
    (dream-autoloads--write-forms file forms)
    (dream-autoloads--write-state state)
    (dream-autoloads--cleanup-obsolete file)
    (message "Generating Dream autoload cache...done")
    file))

(defun dream-autoloads-clean ()
  "Delete Dream's generated autoload cache files.
This is called after Borg clean operations and can also be used interactively.

删除 Dream 生成的聚合 autoload 缓存和状态文件。Borg clean 后会调用，也可
交互式手动调用。"
  (interactive)
  (dream-autoloads--cleanup-obsolete)
  (dolist (file (list dream-autoloads-file
                     dream-autoloads-state-file
                     (concat (file-name-sans-extension dream-autoloads-file)
                             ".elc")))
    (when (file-exists-p file)
      (delete-file file))))

(defun dream-autoloads--borg-batch-rebuild-a (fn &rest args)
  "Around advice for `borg-batch-rebuild' with FN and ARGS.
The advice defers per-drone generation and rebuilds the single cache once after
Borg has rebuilt all drones and init files.

`borg-batch-rebuild' 的 around advice。全量构建期间延迟每个 drone 的生成，
等 Borg 完成所有 drone 和 init 文件构建后只重建一次聚合缓存。"
  (let ((dream-autoloads--defer-generate t))
    (prog1 (apply fn args)
      (if (fboundp 'dream-build-after-borg-rebuild)
          (dream-build-after-borg-rebuild (nth 1 args))
        (dream-autoloads-generate)))))

(defun dream-autoloads--borg-build-a (&rest _args)
  "After advice for `borg-build'.
When a single drone is built outside `borg-batch-rebuild', regenerate the single
cache immediately after Borg succeeds.

`borg-build' 的 after advice。若不是处于全量构建延迟阶段，单个 drone 构建
成功后立即重建聚合缓存。"
  (unless dream-autoloads--defer-generate
    (if (fboundp 'dream-build-refresh-autoloads)
        (dream-build-refresh-autoloads)
      (dream-autoloads-generate))))

(defun dream-autoloads--borg-batch-clean-a (&rest _args)
  "After advice for `borg--batch-clean'.
Remove the single cache after Borg removes package build artifacts.

`borg--batch-clean' 的 after advice。Borg 清理包构建产物后删除聚合缓存。"
  (dream-autoloads-clean))

(defun dream-autoloads-install-borg-advices ()
  "Install Borg build advices that maintain Dream's autoload cache.
This is called in batch Makefile sessions after Borg has been loaded but before
the requested Borg build command runs.

安装维护 Dream 聚合 autoload 缓存的 Borg advice。该函数在 Makefile 的 batch
会话中、Borg 加载完成后、真正的 Borg 构建命令执行前调用。"
  (dream-autoloads-capture-baseline)
  (unless (advice-member-p #'dream-autoloads--borg-batch-rebuild-a
                           'borg-batch-rebuild)
    (advice-add 'borg-batch-rebuild
                :around #'dream-autoloads--borg-batch-rebuild-a))
  (unless (advice-member-p #'dream-autoloads--borg-build-a 'borg-build)
    (advice-add 'borg-build :after #'dream-autoloads--borg-build-a))
  (unless (advice-member-p #'dream-autoloads--borg-batch-clean-a
                           'borg--batch-clean)
    (advice-add 'borg--batch-clean
                :after #'dream-autoloads--borg-batch-clean-a)))

(when noninteractive
  (with-eval-after-load 'borg
    (dream-autoloads-install-borg-advices)))

(provide 'dream-autoloads-build)
;;; dream-autoloads-build.el ends here.
