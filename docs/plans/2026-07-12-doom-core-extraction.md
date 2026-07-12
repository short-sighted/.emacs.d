# Dream Emacs 核心增强计划：从 Doom 核心提取 hook 机制、日志、默认值与便捷宏

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 从 `/Users/jc/Workspace/doomemacs-core` 提取四类能力重建 core：(1) doom-lib.el 的一次性启动 hook 机制（`doom-run-hook-on`），替换 on.el 并摘除对它的全部引用（**on 子模块本身由用户自行移除，不在本计划内**）；(2) doom-emacs.el 的更好默认值；(3) doom 的错误层级定义与 `doom-log` 分级日志；(4) 三组便捷宏（defadvice!/setq-hook!/letf! 的 dream- 前缀版）。

**Architecture:** 两个新 core 文件——`core/dream-hooks.el`（4 个 hook 定义 + `dream-run-hook-on` 机制 + 自装配，完整替代 on.el）与 `core/dream-defaults.el`（doom-emacs.el 默认值提取 + `dream/escape`）；错误定义、`dream-log`、便捷宏并入既有 `core/dream-lib.el`。所有新代码遵守已落地的编译期声明规范（`cl-eval-when (compile)` / Rule 5 存根），由 `make check-isolated` 与 `make check-declare` 两道既有门守护。构建系统零改动（core/ 扫描式编译自动覆盖新文件）。

**Tech Stack:** Emacs 31.0.90, borg v4.5.2, setup.el/once.el（保留）, ERT, make。

## Global Constraints（每个任务都隐含遵守）

- 仅面向 **Emacs 31+**；平台 Windows + macOS；单人配置。
- **命名风格（用户已定）**：全部 dream- 前缀——`dream-defadvice`、`dream-setq-hook`、`dream-letf`、`dream-log`、`dream-first-input-hook`……不引入 doom 叹号名（既有 `quiet!`/`quiet!!` 保持原样，接受其为孤例）。
- 全部自有文件严格编译（`byte-compile-error-on-warn t`）；每个任务结束跑其声明的门；从 T3 起每任务必过 `make check && make config-build && make check-isolated && make smoke`。
- **once.el/once-setup 保留不动**——只移除 on.el；`:once`/`:iload`/`:require-once` 语义不变，仅 hook 符号改名。
- 提交用英语祈使句，**显式 `git add <路径>`（禁止 `git commit -a`，工作树里有大量无关的 drone 子模块状态）**，结尾加 `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`。
- doom 代码为 MIT 许可（on.el 同源同许可），提取改写无合规问题。

---

## Context（背景与已核实事实）

### 为什么移除 on.el（用户判断"on 处理的很差"，逐条核实属实）

on.el（site-lisp/on/on.el，106 行）与 doom `doom-run-hook-on`（doom-lib.el:414）对比：

| 缺陷 | on.el 行为 | doom 机制 |
|---|---|---|
| 无错误处理 | `on-run-first-input-hooks-h` 先 `run-hooks` 再移除触发器（on.el:59-62）——任一 hook 函数出错，`remove-hook` 永不执行，**每次按键重复报错** | `dream-run-hooks` 逐函数 condition-case，signal 携带 hook 名 + 函数名的 `dream-hook-error`；链函数 `running` 闭包防重入，失败也只报一次 |
| 无启动守卫 | 触发器裸挂，启动期间的内部 buffer 操作可能误触 | `after-init-time` 守卫 + `(symbol-value hook)` 非空检查（防 let-bound 屏蔽期误触） |
| 命令行文件失灵 | 触发器在 `window-setup-hook` -100 才装（on.el:102），`emacs foo.txt` 打开的文件**不触发** first-file（recentf 等不启动） | 立即装配 + `after-init-time` 守卫——`after-init-time` 在 command-line-1 处理文件**之前**已置位，命令行文件正常触发（比 doom 还简单：doom 需要 finalize 补射，我们不需要） |
| hook 变量不清零 | 触发后 `on-first-input-hook` 残留旧函数 | 触发后 `(set hook-var nil)` |
| 无谓词 | first-buffer 对 *scratch* 也触发 | predicate 支持（排除 *scratch*，懒加载不被初始 buffer 击穿） |
| 无日志 | 静默，难定位 | 每个 hook 函数经 `dream-log 3` 记录 |

### doom 三件套的语义（已通读源码）

- **`doom-run-hook-on`**（doom-lib.el:414-452）：为 HOOK-VAR 在每个 TRIGGER-HOOK 上装配一次性链函数；`find-file-hook` 特殊处理为 advise `after-find-file :before`（find-file-hook 本身太晚）；daemon 会话把链函数追加到 `server-after-make-frame-hook`（首帧全量加载）。移植时唯一改动：`(not (doom-context-p 'startup))` 守卫换成 `after-init-time`（我们没有也不需要 context 系统）。
- **`doom-log`**（doom-lib.el:186-239）：宏（debug 关闭时参数零求值成本）；`doom-log-level` 0/1/2/3 由 `init-file-debug` 与 `DEBUG` 环境变量决定；`doom--log` 经 `inhibit-message` 控制回显但始终落 *Messages*。移植时剥离 doom-context/module 装饰。
- **错误定义**（doom-lib.el:168-180）：`define-error` 层级，父错误 `doom-error`。我们只取 `dream-error` + `dream-hook-error`（`dream-run-hooks` 依赖），层级可扩展，其余 doom 错误类型与 profile/CLI 绑定，无用。

### doom-emacs.el 默认值提取范围（1891 行已通读，Emacs 31-only 裁剪）

**跳过**：全部向后兼容 backport（1-113 行，31 上全部原生）；`tls-*`（tls.el 已废弃）；`native-comp-async-on-battery-power`（31.0.90 实测 unbound，31.1 才有）；package.el/straight 段；doom 主题/字体加载器（init-ui.el 已有自己的字体系统）；MODE-local-vars-hook 与 switch-{buffer,window,frame} hooks（新机制，超出本计划范围）；autorevert/comint/compile/ediff/so-long 等包级配置（属 lisp/ 单元的事）；C-i/C-m 区分 hack。
**已有不重复**：early-init.el 已有运行时优化块（fast-scrolling、inhibit-compacting-font-caches、菜单栏禁用等）；init-editing.el 已有备份/自动保存/缩进；init-ui.el 已有行号/标题。
**提取**（全部经 31.0.90 batch 实测 boundp/fboundp 编译安全）：UTF-8 编码、gnutls 安全默认（需编译期 `require 'gnutls`——`gnutls-verify-error` 等非预加载）、bidi 性能三件套、文件处理（truename/软链/缺失目录创建/autosave 提示静音）、排版（fill-column/word-wrap/truncate-lines 等，**tabify-regexp 放弃**——tabify.el 非预加载且收益趋零）、帧与窗口（pixelwise/dialog/divider/分屏阈值）、minibuffer 全套（递归/短应答/`y-or-n-p-map` SPC 解绑/光标不可入提示区）、光标与铃声、滚动、uniquify/fringe、macOS/Windows 修饰键（**Rule 5 存根**：`mac-*`/`ns-*`/`w32-*` 均为各自平台 C 变量，对方平台编译器看不见）、`dream/escape` 统一 C-g（用户已选定引入）。

### 现状盘点（迁移面）

- `on` 的引用共 8 文件：core/dream-setup.el:12（require）、core/dream-startup.el:12,99、lisp/init-editing.el:93-96、lisp/init-ui.el:85、lisp/init-tools.el:38、lisp/init-completion.el:17,28,50、test/dream-core-test.el:17,50,56,156-159、test/dream-benchmark.el:36。drone 注册在 .gitmodules:183-186（`[submodule "on"]`，path site-lisp/on）。
- **工作树有用户未提交的编辑**：dream-setup.el 重新加回 `:hooks`/`:load-after`/`:after` 三个 setup 关键字；init-completion.el 的 corfu 改为 `(:hooks on-first-input-hook global-corfu-mode)`。这使既有测试 `dream-setup-pruned-keywords-are-not-defined`（断言 :after/:hooks 未定义）**当前失败**——T0 先修测试、提交基线。
- 当前测试数 **29**（cargo 缺席时 1 skipped）；进度线：T0 29 绿 → T1 31 → T2 34 → T3 34 → T4 36 → T5 39。
- init.el 加载序：dream-paths → dream-startup → dream-autoloads → dream-lib → dream-setup → 各单元 → dream-startup-initialize。新序：dream-startup 顶部 require dream-hooks（其 require dream-lib），init.el 显式列出 dream-hooks 并在 dream-setup 后插入 dream-defaults。
- 构建为扫描式：`dream-build-config-files` 自动收录 core/ 新文件（测试 `dream-build-file-set-scans-lisp-tree` 已证），Makefile/构建脚本**零改动**。
- 编译安全实测（`emacs -Q --batch` boundp/fboundp）：`after-init-time`=t（batch 也置位，测试守卫需 let-bind nil）、`x-stretch-cursor`=t、`lwarn`=t、`keymap-unset`=t、`run-hook-wrapped`=t、`window-buffer-change-functions`=t、`y-or-n-p-map`=t、uniquify/show-paren/mwheel/window-divider 变量全预加载；`gnutls-min-prime-bits`=nil（需编译期 require gnutls）、`string-remove-suffix`=nil（subr-x 非预加载，dream-lib 需 require subr-x）、`w32-lwindow-modifier`=nil（mac 上）。

### 设计定案（用户四问已答）

1. 额外宏取：`dream-defadvice`/`dream-undefadvice`、`dream-setq-hook`/`dream-unsetq-hook`、`dream-letf`；不取 `fn!`/`cmd!`/rpartial（键位少收益低）、`add-transient-hook!`（与 once.el 重复）、`after!`（与 cl-eval-when 编译期规范冲突）。
2. 命名全 dream- 前缀。
3. 引入 `dream/escape` + `dream-escape-hook`。
4. 布局：新增 core/dream-hooks.el + core/dream-defaults.el；错误/日志/宏进 core/dream-lib.el。

### 行为变化清单（除此以外运行期语义不变）

1. first-input/file/buffer 触发获得错误隔离、启动守卫、命令行文件支持、hook 清零（见上表）。
2. **init-ui 时机后移**：on.el 在 `after-init-hook` 跑 on-init-ui-hook；新机制在 `window-setup-hook` -100（doom 实测的性能甜点，启动序最末）。daemon 仍为 `server-after-make-frame-hook`。benchmark 门负责验证无回归。
3. first-buffer 增加谓词：`*scratch*` 不再触发（global-so-long-mode 等推迟到首个真实 buffer）。
4. dream-defaults.el 引入的新默认值（UTF-8、gnutls、minibuffer 等——为"更好默认值"本身）与 `dream/escape` 接管 C-g。

---

## 阶段总览

| 阶段 | 任务 | 阶段目标 | 验收标准 |
|---|---|---|---|
| **阶段 0 · 基线** | T0 | 工作树回到全绿可提交状态；计划入库 | `make check` 29/29；用户 WIP + 计划已提交 |
| **阶段 1 · dream-lib 地基** | T1 | 错误层级 + dream-log 落地 | ERT 29→31；严格编译过 |
| **阶段 2 · hook 机制与迁移** | T2, T3 | dream-hooks.el 替代 on.el；全部引用改名 | ERT 31→34；`grep on-first\|on-init` 零命中；四连门全绿 |
| **阶段 3 · 更好默认值** | T4 | dream-defaults.el + dream/escape 接入 init.el | ERT 34→36；四连门 + benchmark 无回归 |
| **阶段 4 · 便捷宏** | T5 | 三组宏入 dream-lib | ERT 36→39 |
| **阶段 5 · 文档与终验** | T6 | conventions.md 新章、README；端到端验证与计时 | 全部门绿；执行记录填写 |

每阶段结束 = 可安全停留：配置可启动、测试全绿、可独立评审。

---

# 阶段 0 · 基线

## Task 0: 基线归零 + 计划入库

**目标：** 工作树含用户自己的未提交编辑（dream-setup 关键字回归、corfu 改挂 on-first-input-hook），且它使 `dream-setup-pruned-keywords-are-not-defined` 失败。修测试使其匹配用户的新意图，连同 WIP 与本计划一并提交，得到绿色基线。

**Files:**
- Modify: `test/dream-core-test.el:124-128`
- Create: `docs/plans/2026-07-12-doom-core-extraction.md`（本计划全文复制）
- Commit: 用户 WIP（core/dream-setup.el、lisp/init-completion.el）

- [ ] **Step 1: 计划入库确认** — 本计划已于规划阶段写入 `docs/plans/2026-07-12-doom-core-extraction.md`（1078 行）；确认与批准版本一致（有修订则重新复制），随本任务一并提交。

- [ ] **Step 2: 修既有测试** — `test/dream-core-test.el` 把 124-128 行：

```elisp
(ert-deftest dream-setup-pruned-keywords-are-not-defined ()
  (require 'dream-setup)
  (dolist (keyword '(:after :opt :hooks :init :bind-map))
    (should-not (assq keyword setup-macros)))
  (should (assq :global setup-macros)))
```

替换为（`:hooks`/`:after`/`:load-after` 已被用户有意加回，转为正向断言）：

```elisp
(ert-deftest dream-setup-pruned-keywords-are-not-defined ()
  (require 'dream-setup)
  (dolist (keyword '(:opt :init :bind-map))
    (should-not (assq keyword setup-macros)))
  (dolist (keyword '(:global :hooks :after :load-after))
    (should (assq keyword setup-macros))))
```

- [ ] **Step 3:** `make check`。期望：`Ran 29 tests, 29 results as expected`（无 cargo 机器 1 skipped，下同不再重复）。
- [ ] **Step 4:** 顺手清理 init-completion.el 第 71-73 行 WIP 引入的尾随空白（corfu 块结尾与 `dream-completion-add-file-capf` 之间应只有一个空行）。
- [ ] **Step 5: Commit** —

```bash
git add core/dream-setup.el lisp/init-completion.el test/dream-core-test.el \
        docs/plans/2026-07-12-doom-core-extraction.md
git commit -m "Restore setup keywords and record doom-core extraction plan"
```

（提交信息尾行加 `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`，下同。）

---

# 阶段 1 · dream-lib 地基

## Task 1: 错误层级 + dream-log

**目标：** `core/dream-lib.el` 增加 `dream-error`/`dream-hook-error` 定义与 `dream-log` 分级日志（doom-lib.el:168-239 移植，剥离 context 装饰）。日志级别：0 无 / 1 警告 / 2 通知 / 3 调试；交互会话默认 0，`--debug-init` 时 2，`DEBUG=N` 环境变量可指定；batch 会话固定 3（但非 debug 时 inhibit-message）。

**Files:**
- Modify: `core/dream-lib.el`（第 3 行 `(require 'cl-lib)` 之后插入）
- Modify: `test/dream-core-test.el`（`(provide 'dream-core-test)` 之前追加 2 个测试）

**Interfaces:**
- Produces: `(dream-log [LEVEL] MESSAGE ARGS...)` 宏——LEVEL 整数可选默认 2，关闭时参数不求值；`dream-error`（父错误）、`dream-hook-error`（T2 的 `dream-run-hooks` 依赖）；`dream-inhibit-log`、`dream-log-level` 变量。

- [ ] **Step 1: 写失败测试** — `test/dream-core-test.el` 末尾追加：

```elisp
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
```

- [ ] **Step 2:** `make check`。期望：`Ran 31 tests, 29 results as expected, 2 unexpected`，失败原因 `(void-variable dream-inhibit-log)` 与 error-conditions 不匹配。
- [ ] **Step 3: 实现** — `core/dream-lib.el` 在 `(require 'cl-lib)` 之后插入：

```elisp
(require 'subr-x)

;;; Errors

(define-error 'dream-error "An unexpected Dream Emacs error")
(define-error 'dream-hook-error "Error in a Dream Emacs startup hook"
              'dream-error)

;;; Logging

(defvar dream-inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `dream-log' output completely.")

(defvar dream-log-level
  (if noninteractive
      3
    (if init-file-debug
        (if-let* ((level (getenv-internal "DEBUG"))
                  (level (if (string-empty-p level) 1 (string-to-number level)))
                  ((not (zerop level))))
            level
          2)
      0))
  "Verbosity of `dream-log' calls.
0 -- No logging at all.
1 -- Only warnings.
2 -- Warnings and notices.
3 -- Debug info, warnings, and notices.")

(defun dream--log (level text &rest args)
  "Emit TEXT formatted with ARGS at LEVEL into *Messages*.
Levels above `dream-log-level' stay out of the echo area."
  (let ((inhibit-message (if noninteractive
                             (not init-file-debug)
                           (> level dream-log-level))))
    (apply #'message
           (propertize (concat "* %.06f: " text) 'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           args)))

(defmacro dream-log (message &rest args)
  "Log MESSAGE (a format string applied to ARGS) when logging is on.
An integer in MESSAGE's position selects the level (default 2).
Expands to a guarded call, so ARGS are not evaluated while logging
is disabled."
  (declare (debug t))
  (let ((level (if (integerp message)
                   (prog1 message
                     (setq message (pop args)))
                 2)))
    `(when (and (not dream-inhibit-log)
                (or (not noninteractive)
                    (<= ,level dream-log-level)))
       (dream--log ,level ,message ,@args))))
```

（`subr-x` 是 Rule 1 运行期真依赖——T5 的 `string-remove-suffix` 也用它，一次到位；`getenv-internal` 为 C 函数免声明。）

- [ ] **Step 4:** `make check`。期望：`Ran 31 tests, 31 results as expected`。
- [ ] **Step 5:** `make config-build && make check-isolated`。期望零警告、隔离检查静默通过。
- [ ] **Step 6: Commit** —

```bash
git add core/dream-lib.el test/dream-core-test.el
git commit -m "Add dream error hierarchy and leveled logging"
```

---

# 阶段 2 · hook 机制与迁移

## Task 2: core/dream-hooks.el

**目标：** 新文件承载 4 个一次性启动 hook（dream-first-input/file/buffer、dream-init-ui）与 doom 移植的运行机制。交互会话加载即自装配（`unless noninteractive`），batch/测试不装配。此任务只**新建**，不动任何消费者——on.el 与新机制并存，互不冲突。

**Files:**
- Create: `core/dream-hooks.el`
- Modify: `test/dream-core-test.el`（文件顶部 defvar 区追加 2 个测试变量；末尾追加 3 个测试）

**Interfaces:**
- Produces: hook 变量 `dream-first-input-hook`、`dream-first-file-hook`、`dream-first-buffer-hook`、`dream-init-ui-hook`；`(dream-run-hooks &rest HOOKS)`（错误升格为携带 hook 名+函数名的 `dream-hook-error`）；`(dream-run-hook-on HOOK-VAR TRIGGER-HOOKS &optional PREDICATE)`；`(dream-hooks-arm)`。
- Consumes: T1 的 `dream-log`、`dream-hook-error`。

- [ ] **Step 1: 写失败测试** — `test/dream-core-test.el` 顶部 defvar 区（第 12 行 `(defvar rust-ts-mode-hook nil)` 之后）追加：

```elisp
(defvar dream-test-transient-hook nil)
(defvar dream-test-trigger-a-hook nil)
```

文件末尾追加：

```elisp
(ert-deftest dream-hooks-transient-hook-fires-once-then-disarms ()
  (require 'dream-hooks)
  (let ((dream-test-transient-hook nil)
        (dream-test-trigger-a-hook nil)
        (after-init-time (current-time))
        (calls 0))
    (add-hook 'dream-test-transient-hook (lambda () (cl-incf calls)))
    (dream-run-hook-on 'dream-test-transient-hook '(dream-test-trigger-a-hook))
    (run-hooks 'dream-test-trigger-a-hook)
    (run-hooks 'dream-test-trigger-a-hook)
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

(ert-deftest dream-hooks-errors-name-the-hook-and-function-then-disarm ()
  (require 'dream-hooks)
  (let* ((dream-test-transient-hook nil)
         (dream-test-trigger-a-hook nil)
         (after-init-time (current-time))
         (boom (lambda () (error "boom"))))
    (add-hook 'dream-test-transient-hook boom)
    (dream-run-hook-on 'dream-test-transient-hook '(dream-test-trigger-a-hook))
    (let ((failure (should-error (run-hooks 'dream-test-trigger-a-hook)
                                 :type 'dream-hook-error)))
      (should (eq (nth 1 failure) 'dream-test-transient-hook))
      (should (eq (nth 2 failure) boom)))
    ;; A failed attempt must not leave the trigger armed.
    (run-hooks 'dream-test-trigger-a-hook)))
```

（第三个测试会在 batch 输出一行 lwarn 告警文本——预期噪声，非失败。）

- [ ] **Step 2:** `make check`。期望：`Ran 34 tests, 31 results as expected, 3 unexpected`，失败原因 `(file-missing ... "dream-hooks")`。
- [ ] **Step 3: 实现** — 新建 `core/dream-hooks.el`，完整内容：

```elisp
;;; dream-hooks.el --- Deferred startup hooks for Dream Emacs. -*- lexical-binding: t; -*-

(require 'dream-lib)

(defvar dream-first-input-hook nil
  "Transient hooks run before the first user input.")

(defvar dream-first-file-hook nil
  "Transient hooks run before the first interactively opened file.")

(defvar dream-first-buffer-hook nil
  "Transient hooks run before the first interactively opened buffer.")

(defvar dream-init-ui-hook nil
  "Transient hooks run once the first usable frame exists.")

(defvar dream-hooks--current nil
  "The hook variable `dream-run-hooks' is currently running.")

(defun dream-run-hook (hook)
  "Call HOOK (a function) and upgrade errors to `dream-hook-error'.
Meant for `run-hook-wrapped'; always returns nil so iteration
continues."
  (dream-log 3 "hook:%s: run %s" (or dream-hooks--current '*) hook)
  (condition-case-unless-debug error
      (funcall hook)
    (error
     (signal 'dream-hook-error (list hook error))))
  nil)

(defun dream-run-hooks (&rest hooks)
  "Run each hook variable in HOOKS, naming hook and function on error."
  (dolist (hook hooks)
    (condition-case-unless-debug error
        (let ((dream-hooks--current hook))
          (run-hook-wrapped hook #'dream-run-hook))
      (dream-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (nth 1 error) (nth 2 error)))
       (signal 'dream-hook-error (cons hook (cdr error)))))))

(defun dream-run-hook-on (hook-var trigger-hooks &optional predicate)
  "Run HOOK-VAR exactly once when any of TRIGGER-HOOKS first fires.
Fires only after Emacs has initialized (`after-init-time') and only
when PREDICATE (if any) returns non-nil.  HOOK-VAR is reset to nil
afterwards, and each chain attempts at most once even on error."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "dream-chain-%s-to-%s" hook-var hook)))
          running)
      (fset
       fn (lambda (&rest _)
            (when (and (not running)
                       after-init-time
                       (or (daemonp)
                           ;; Hooks may be let-bound to nil during internal
                           ;; batch operations; treat that as non-interactive.
                           (and (boundp hook) (symbol-value hook)))
                       (or (null predicate)
                           (funcall predicate)))
              (setq running t)
              (dream-run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        ;; Daemon sessions skip the lazy-loading dance; load everything
        ;; when the first frame arrives.
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          ;; `find-file-hook' runs after the buffer is fully initialized,
          ;; which is too late; fire just before instead.
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))

(defun dream-hooks--run-init-ui (&rest _)
  "Run `dream-init-ui-hook' once, then disarm."
  (dream-run-hooks 'dream-init-ui-hook)
  (setq dream-init-ui-hook nil)
  (remove-hook 'window-setup-hook #'dream-hooks--run-init-ui)
  (remove-hook 'server-after-make-frame-hook #'dream-hooks--run-init-ui))

(defun dream-hooks-arm ()
  "Install the transient startup hook triggers for this session."
  (dream-run-hook-on 'dream-first-file-hook
                     '(find-file-hook dired-initial-position-hook))
  (dream-run-hook-on 'dream-first-input-hook '(pre-command-hook))
  (dream-run-hook-on 'dream-first-buffer-hook
                     '(find-file-hook window-buffer-change-functions
                       server-visit-hook)
                     (lambda ()
                       (not (equal (buffer-name) "*scratch*"))))
  (add-hook (if (daemonp) 'server-after-make-frame-hook 'window-setup-hook)
            #'dream-hooks--run-init-ui -100))

(unless noninteractive
  (dream-hooks-arm))

(provide 'dream-hooks)
;;; dream-hooks.el ends here.
```

实现要点（评审核对）：`make-symbol` + `fset` 与 doom 一致（未 intern，卸载即消失）；`running` 为词法闭包变量，错误路径下 hook-var 不清零但链已熄火——只报一次；`after-init-time` 在 command-line-1 之前置位，命令行文件可正常触发 first-file（on.el 做不到）；daemon 分支与 `find-file-hook`→`after-find-file` 特判逐字保留 doom 语义。

- [ ] **Step 4:** `make check`。期望：`Ran 34 tests, 34 results as expected`。
- [ ] **Step 5:** `make config-build && make check-isolated`。新文件被扫描式构建自动收录，严格编译零警告。
- [ ] **Step 6: Commit** —

```bash
git add core/dream-hooks.el test/dream-core-test.el
git commit -m "Add dream-hooks transient startup hook machinery"
```

## Task 3: 全量迁移 on-* → dream-*

**目标：** 8 处消费者全部改挂 dream-* hook；dream-setup 摘除 `(require 'on)`；init.el 显式接入 dream-hooks。改动全部是机械替换，运行期语义变化仅为 Context"行为变化清单"所列。

**Files:**
- Modify: `init.el:16-17`、`core/dream-setup.el:12`、`core/dream-startup.el:3-4,12,99`、`lisp/init-editing.el:93-96`、`lisp/init-ui.el:85`、`lisp/init-tools.el:38`、`lisp/init-completion.el:17,28,50`、`test/dream-core-test.el:17,50,56,156-159`、`test/dream-benchmark.el:36`

- [ ] **Step 1:** `core/dream-setup.el` 删除第 12 行：

```elisp
(require 'on)
```

- [ ] **Step 2:** `core/dream-startup.el` 三处：
  - 第 3-4 行的

    ```elisp
    (require 'dream-paths)
    (require 'cl-lib)
    ```

    改为

    ```elisp
    (require 'dream-paths)
    (require 'dream-hooks)
    (require 'cl-lib)
    ```

  - 删除第 12 行 `(defvar on-init-ui-hook nil)`（保留第 13 行 `dream-startup--incremental-loading-started` 的 defvar）。
  - 第 99 行

    ```elisp
    (add-hook 'on-init-ui-hook #'dream-startup--start-incremental-loading -100)
    ```

    改为

    ```elisp
    (add-hook 'dream-init-ui-hook #'dream-startup--start-incremental-loading -100)
    ```

- [ ] **Step 3:** `init.el` 第 16-17 行：

```elisp
  ;; Core
  (require 'dream-lib)
  (require 'dream-setup)
```

改为：

```elisp
  ;; Core
  (require 'dream-lib)
  (require 'dream-hooks)
  (require 'dream-setup)
```

- [ ] **Step 4:** lisp/ 四文件机械替换：
  - `lisp/init-editing.el:93-96`：

    ```elisp
    (add-hook 'dream-first-input-hook #'delete-selection-mode -90)
    (add-hook 'dream-first-file-hook #'recentf-mode -90)
    (add-hook 'dream-first-file-hook #'global-auto-revert-mode -80)
    (add-hook 'dream-first-buffer-hook #'global-so-long-mode -90)
    ```

  - `lisp/init-ui.el:85`：`(add-hook 'dream-init-ui-hook #'dream-ui-initialize)`
  - `lisp/init-tools.el:38`：`(add-hook 'dream-init-ui-hook #'dream-tools-initialize-shell-environment -80)`
  - `lisp/init-completion.el` 三处：
    - 第 17 行：`(list :hook 'dream-first-input-hook :depth -70))`
    - 第 28 行：`(:require-once (list :hooks 'dream-first-input-hook) 'orderless)`
    - 第 50 行：`(:hooks dream-first-input-hook global-corfu-mode)`

- [ ] **Step 5:** 测试与 benchmark：
  - `test/dream-core-test.el:17` 删除整行（`../site-lisp/on` 的 load-path 追加）。
  - `:50` `(on-init-ui-hook nil)` → `(dream-init-ui-hook nil)`；`:56` `(run-hooks 'on-init-ui-hook)` → `(run-hooks 'dream-init-ui-hook)`。
  - `:156-159` 四个 let 绑定 `on-first-input-hook`/`on-first-file-hook`/`on-first-buffer-hook`/`on-init-ui-hook` → 对应 dream-* 名。
  - `test/dream-benchmark.el:36`：`(lambda () (run-hooks 'on-first-input-hook))` → `(lambda () (run-hooks 'dream-first-input-hook))`。

- [ ] **Step 6: 残留清点** —

```bash
grep -rn "on-first-input\|on-first-file\|on-first-buffer\|on-init-ui\|(require 'on)" \
  --include='*.el' init.el core/ lisp/ lib/ test/ build/
```

期望：零输出。

- [ ] **Step 7: 四连门** — `make check && make config-build && make check-isolated && make smoke`。期望：`Ran 34 tests, 34 results as expected`；严格编译零警告；smoke 输出 `Dream smoke: N features, ...` 行。此后 on drone 仍安装（autoloads 里仍注册）但已无人引用——**子模块摘除由用户自行处理，本计划不动它**。
- [ ] **Step 8: Commit** —

```bash
git add init.el core/dream-setup.el core/dream-startup.el \
        lisp/init-editing.el lisp/init-ui.el lisp/init-tools.el \
        lisp/init-completion.el test/dream-core-test.el test/dream-benchmark.el
git commit -m "Migrate startup hooks from on.el to dream-hooks"
```

---

# 阶段 3 · 更好默认值

## Task 4: core/dream-defaults.el + dream/escape

**目标：** doom-emacs.el 默认值提取为新文件并接入 init.el（dream-setup 之后、编辑单元之前，单元可覆盖默认值）。含用户选定的 `dream/escape`。

**Files:**
- Create: `core/dream-defaults.el`
- Modify: `init.el`（`(require 'dream-setup)` 之后插一行）
- Modify: `test/dream-core-test.el`（末尾追加 2 个测试）

**Interfaces:**
- Produces: `dream-escape-hook` 变量、`dream/escape` 命令（remap `keyboard-quit`）；其余为纯默认值设置。
- Consumes: T2 的 `dream-init-ui-hook`（window-divider-mode 挂接）。

- [ ] **Step 1: 写失败测试** — `test/dream-core-test.el` 末尾追加：

```elisp
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
    (dream/escape)
    (should (equal (nreverse order) '(first second))))
  (let ((dream-escape-hook nil))
    (should (eq 'quit (condition-case nil
                          (progn (dream/escape) nil)
                        (quit 'quit))))))
```

- [ ] **Step 2:** `make check`。期望：`Ran 36 tests, 34 results as expected, 2 unexpected`，失败原因 `(file-missing ... "dream-defaults")`。
- [ ] **Step 3: 实现** — 新建 `core/dream-defaults.el`，完整内容：

```elisp
;;; dream-defaults.el --- Better baseline defaults. -*- lexical-binding: t; -*-

(require 'dream-hooks)

(cl-eval-when (compile)
  ;; `gnutls-verify-error' and friends are not preloaded.
  (require 'gnutls))

;; Rule 5: platform modifier variables live in each build's C code; the
;; other platform's compiler cannot require them from anywhere.
(defvar mac-command-modifier)
(defvar mac-option-modifier)
(defvar mac-right-option-modifier)
(defvar ns-command-modifier)
(defvar ns-option-modifier)
(defvar ns-right-option-modifier)
(defvar w32-lwindow-modifier)
(defvar w32-rwindow-modifier)

;;; Encodings

(set-language-environment "UTF-8")
;; `set-language-environment' also sets `default-input-method'; undo that.
(setq default-input-method nil)
;; The Windows clipboard tends to be UTF-16; leave it alone there.
(unless (eq system-type 'windows-nt)
  (setq selection-coding-system 'utf-8))

;;; Security

(setq gnutls-verify-error noninteractive
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not (eq system-type 'windows-nt))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      gnutls-min-prime-bits 3072)

;;; Redisplay performance

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;;; Files

(setq find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t)

(defun dream-defaults-create-missing-directories ()
  "Offer to create the parent directories of a nonexistent visited file."
  (unless (file-remote-p buffer-file-name)
    (let ((parent (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent))
           (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                             parent))
           (progn (make-directory parent 'parents) t)))))

(add-hook 'find-file-not-found-functions
          #'dream-defaults-create-missing-directories)

(defun dream-defaults--quiet-autosave-notice (function &rest args)
  "Call FUNCTION with ARGS without pausing on auto-save-data notices."
  (cl-letf (((symbol-function 'sit-for) #'ignore))
    (apply function args)))

(advice-add 'after-find-file :around #'dream-defaults--quiet-autosave-notice)

;;; Formatting

(setq-default fill-column 80
              word-wrap t
              truncate-lines t)
(setq truncate-partial-width-windows nil
      sentence-end-double-space nil
      require-final-newline t)
(add-hook 'text-mode-hook #'visual-line-mode)

;;; Frames and windows

(setq frame-resize-pixelwise t
      window-resize-pixelwise nil
      use-dialog-box nil
      split-width-threshold 160
      split-height-threshold nil
      window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'dream-init-ui-hook #'window-divider-mode)

;;; Minibuffer

(setq read-extended-command-predicate #'command-completion-default-include-p
      enable-recursive-minibuffers t
      echo-keystrokes 0.02
      resize-mini-windows 'grow-only
      use-short-answers t
      minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
;; SPC-as-yes is too easy to hit by accident.
(keymap-unset y-or-n-p-map "SPC")
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;; Cursor and bells

(blink-cursor-mode -1)
(setq blink-matching-paren nil
      x-stretch-cursor nil
      ring-bell-function #'ignore
      visible-bell nil)

;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;;; Buffers and fringes

(setq uniquify-buffer-name-style 'forward
      confirm-nonexistent-file-or-buffer nil
      mouse-yank-at-point t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;;; Matching parens (`show-paren-mode' is global by default since 28)

(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

;;; Platform modifiers

(cond
 ((eq system-type 'darwin)
  (setq mac-command-modifier 'super
        ns-command-modifier 'super
        mac-option-modifier 'meta
        ns-option-modifier 'meta
        mac-right-option-modifier 'none
        ns-right-option-modifier 'none))
 ((eq system-type 'windows-nt)
  (setq w32-lwindow-modifier 'super
        w32-rwindow-modifier 'super)))

;;; Universal escape

(defvar dream-escape-hook nil
  "Hook run by `dream/escape' before falling back to `keyboard-quit'.
When any function returns non-nil, the remaining functions and the
fallback are skipped.")

(defun dream/escape (&optional interactive)
  "Quit the minibuffer, run `dream-escape-hook', or `keyboard-quit'."
  (interactive (list 'interactive))
  (let ((inhibit-quit t))
    (cond ((minibuffer-window-active-p (minibuffer-window))
           (when interactive
             (setq this-command 'abort-recursive-edit))
           (abort-recursive-edit))
          ((run-hook-with-args-until-success 'dream-escape-hook))
          ;; Never interrupt macro recording or replay.
          ((or defining-kbd-macro executing-kbd-macro) nil)
          ((unwind-protect (keyboard-quit)
             (when interactive
               (setq this-command 'keyboard-quit)))))))

(keymap-global-set "<remap> <keyboard-quit>" #'dream/escape)

(provide 'dream-defaults)
;;; dream-defaults.el ends here.
```

- [ ] **Step 4:** `init.el` 在 `(require 'dream-setup)` 之后插入：

```elisp
  (require 'dream-defaults)
```

- [ ] **Step 5:** `make check`。期望：`Ran 36 tests, 36 results as expected`。
- [ ] **Step 6: 四连门 + benchmark** — `make config-build && make check-isolated && make smoke && make benchmark`。benchmark p50/p95 相对基线 <10% 波动（defaults 是纯 setq + 3 个 add-hook + 1 个 advice，理论近零；init-ui 后移到 window-setup 也在此得到验证）。若 `make benchmark` 因基线归属不同 Emacs 报错，先 `make benchmark-baseline` 重建再跑。
- [ ] **Step 7: Commit** —

```bash
git add core/dream-defaults.el init.el test/dream-core-test.el
git commit -m "Add dream-defaults with doom-derived baseline and escape"
```

---

# 阶段 4 · 便捷宏

## Task 5: dream-letf / dream-setq-hook / dream-defadvice

**目标：** 三组宏（用户选定）入 `core/dream-lib.el`，doom 原语义、dream- 命名。相对 doom 的简化：`dream-letf` 的 defun 分支用普通 `lambda`（不引入 `lambda!`/cl-arglist），defadvice 分支仅保留 `(defadvice TARGET WHERE FN)` 关键字形式（不移植 define-advice 变体）。

**Files:**
- Modify: `core/dream-lib.el`（`(provide 'dream-lib)` 之前插入）
- Modify: `test/dream-core-test.el`（顶部 defvar 区 +2 变量；末尾 +3 测试）

**Interfaces:**
- Produces: `(dream-letf BINDINGS BODY...)`、`(dream-setq-hook HOOKS [SYM VAL]...)`、`(dream-unsetq-hook HOOKS SYMS...)`、`(dream-defadvice SYMBOL ARGLIST [DOC] [WHERE PLACES]... BODY...)`、`(dream-undefadvice ...)`。
- Consumes: dream-lib 既有 `dream--resolve-hook-forms`；T1 引入的 `(require 'subr-x)`（`string-remove-suffix`）。

- [ ] **Step 1: 写失败测试** — `test/dream-core-test.el` 顶部 defvar 区追加：

```elisp
(defvar dream-test-mode-hook nil)
(defvar dream-test-local-var)
```

末尾追加：

```elisp
(ert-deftest dream-setq-hook-sets-buffer-local-values ()
  (require 'dream-lib)
  (let ((dream-test-mode-hook nil))
    (dream-setq-hook dream-test-mode dream-test-local-var 42)
    (with-temp-buffer
      (run-hooks 'dream-test-mode-hook)
      (should (local-variable-p 'dream-test-local-var))
      (should (= dream-test-local-var 42)))
    (dream-unsetq-hook dream-test-mode dream-test-local-var)
    (should-not dream-test-mode-hook)))

(ert-deftest dream-defadvice-defines-and-attaches-advice ()
  (require 'dream-lib)
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
  (require 'dream-lib)
  (defun dream-test-letf-target () 'real)
  (should (eq (dream-letf ((defun dream-test-letf-target () 'fake))
                (dream-test-letf-target))
              'fake))
  (should (eq (dream-test-letf-target) 'real)))
```

- [ ] **Step 2:** `make check`。期望：`Ran 39 tests, 36 results as expected, 3 unexpected`（`invalid-function`/`void-function` 系失败）。
- [ ] **Step 3: 实现** — `core/dream-lib.el` 在 `(provide 'dream-lib)` 之前插入：

```elisp
;;; Temporary bindings

(defmacro dream-letf (bindings &rest body)
  "Temporarily rebind functions, macros, or advice around BODY.
BINDINGS is a list of (PLACE VALUE) pairs as for `cl-letf*', or one
or more of these definition forms:

  (defun NAME (ARGS...) BODY...)     temporary function via `cl-letf'
  (defun* NAME (ARGS...) BODY...)    recursive function via `cl-labels'
  (defmacro NAME (ARGS...) BODY...)  temporary macro via `cl-macrolet'
  (defadvice FUNCTION WHERE ADVICE)  advice removed again afterwards"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq body
            (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice
               (cl-destructuring-bind (target where fn) rest
                 `(when-let* ((fn ,fn))
                    (advice-add ,target ,where fn)
                    (unwind-protect ,body (advice-remove ,target fn)))))
              (`defun
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  (cl-letf (((symbol-function #',(car rest))
                             (lambda ,(cadr rest) ,@(cddr rest))))
                    ,body)))
              (`defun*
               `(cl-labels ((,@rest)) ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

;;; Hook-local variables

(defun dream--setq-hook-forms (hooks rest &optional singles)
  "Return (VAR VAL HOOK FN) tuples for `dream-setq-hook' over HOOKS.
REST is the flat [SYM VAL]... list, or bare symbols when SINGLES."
  (unless (or singles (zerop (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list 'evenp (length rest))))
  (cl-loop with vars = (let ((args rest) vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (dream--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect (list var val hook
                                  (intern (format "dream--setq-%s-for-%s"
                                                  var mode))))))

(defmacro dream-setq-hook (hooks &rest var-vals)
  "Set buffer-local VAR-VALS whenever any of HOOKS run.
\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (dream--setq-hook-forms hooks var-vals)
            collect `(defun ,fn (&rest _) (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defmacro dream-unsetq-hook (hooks &rest vars)
  "Remove the setters installed by `dream-setq-hook' for VARS.
\(fn HOOKS &rest SYM...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (dream--setq-hook-forms hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))

;;; Advice definers

(defmacro dream-defadvice (symbol arglist &optional docstring &rest body)
  "Define advice SYMBOL and attach it to the WHERE/PLACES pairs in BODY.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body))) where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

(defmacro dream-undefadvice (symbol _arglist &optional docstring &rest body)
  "Remove advice SYMBOL from the WHERE/PLACES pairs in BODY.
\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY)"
  (declare (doc-string 3) (indent defun))
  (let (where-alist)
    (unless (stringp docstring)
      (push docstring body))
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body))) where-alist))
    `(dolist (targets (list ,@(nreverse where-alist)))
       (dolist (target (cdr targets))
         (advice-remove target #',symbol)))))
```

- [ ] **Step 4:** `make check`。期望：`Ran 39 tests, 39 results as expected`。
- [ ] **Step 5:** `make config-build && make check-isolated` 全绿。
- [ ] **Step 6: Commit** —

```bash
git add core/dream-lib.el test/dream-core-test.el
git commit -m "Add dream-letf, dream-setq-hook, and dream-defadvice macros"
```

---

# 阶段 5 · 文档与终验

## Task 6: 规范成文 + 端到端验证

**Files:**
- Modify: `docs/conventions.md`（末尾追加）、`README.md`、`docs/plans/2026-07-12-doom-core-extraction.md`（执行记录）

- [ ] **Step 1:** `docs/conventions.md` 末尾追加整章：

```markdown
## 启动钩子与日志规范

### 一次性启动钩子（core/dream-hooks.el）

按启用时机选择挂接点，全部由 `dream-run-hook-on` 驱动：触发一次、
hook 变量清零、错误升格为携带 hook 名与函数名的 `dream-hook-error`，
且失败的链不会重复触发。

| 钩子 | 触发时机 | 适用 |
|---|---|---|
| `dream-first-input-hook` | 首次用户输入前 | 输入相关全局模式（补全、delete-selection） |
| `dream-first-file-hook` | 首次交互式打开文件前（含命令行文件） | 文件编辑设施（recentf、auto-revert） |
| `dream-first-buffer-hook` | 首个真实 buffer（排除 *scratch*） | buffer 级全局模式（so-long） |
| `dream-init-ui-hook` | 首个可用帧（window-setup / daemon 首帧） | 帧相关初始化（modeline、字体、shell env） |

daemon 会话不做懒加载：全部链函数在首帧一并触发。

### 日志（dream-log）

`(dream-log [LEVEL] FORMAT ARGS...)`——LEVEL 缺省 2；日志关闭时参数
不求值。交互会话默认静默；`emacs --debug-init` 打开级别 2，
`DEBUG=3 emacs --debug-init` 打开调试级；batch 会话恒为级别 3。
输出永远落 *Messages*，带启动相对时间戳。

### 错误

自有错误一律挂在 `dream-error` 层级下（`define-error ... 'dream-error`），
调用方可统一 `condition-case` 捕获。现有子类：`dream-hook-error`。

### 便捷宏（core/dream-lib.el）

- `dream-add-hook`：N 函数 × M 钩子批量挂接（:append/:local/:depth/:remove）。
- `dream-setq-hook` / `dream-unsetq-hook`：按 mode hook 设置 buffer-local 值，
  生成可 `C-h f` 定位的具名函数。
- `dream-defadvice` / `dream-undefadvice`：定义并挂接 advice 一步完成。
- `dream-letf`：测试与临时场景下重绑函数/宏/advice。
- `quiet!` / `quiet!!`：静默求值（历史命名，保留）。
```

- [ ] **Step 2:** `README.md`：core 文件清单补 `dream-hooks.el` 与 `dream-defaults.el` 一行说明（措辞与既有条目一致）；若 README 提及 on.el 则删除。
- [ ] **Step 3: 全量门** —

```bash
make check && make config-build && make check-isolated && make check-declare \
  && make smoke && make benchmark
```

全绿；`Ran 39 tests, 39 results as expected`。

- [ ] **Step 4: 交互冒烟**（GUI 启动逐项确认）：
  1. 启动后 modeline/字体正常（`dream-init-ui-hook` 经 window-setup 触发）。
  2. `emacs some.txt` 直接带文件启动：`M-x recentf-mode` 显示已启用（first-file 对命令行文件生效——on.el 时代的缺陷修复）。
  3. 首次按键后 `C-h v dream-first-input-hook` 值为 nil（触发后清零）。
  4. `C-g` 行为如常（minibuffer 退出、宏录制不被打断）。
  5. `DEBUG=3 emacs --debug-init` 启动，*Messages* 出现 `* 0.xxxxx: hook:...` 日志行。
  6. macOS 上 Command=super 生效（如 `s-<tab>` hideshow 键位仍可用）。
- [ ] **Step 5: 负向验证（机制的牙齿）** — scratch 里求值：

```elisp
(let ((dream-test-transient-hook (list (lambda () (error "boom"))))
      (after-init-time (current-time)))
  (dream-run-hook-on 'dream-test-transient-hook '(post-command-hook)))
```

随后一次按键应得到一条指名 `dream-test-transient-hook` 与出错函数的 warning + `dream-hook-error`，且**后续按键不再重复报错**（on.el 会每键一报）。验证后重启会话清理。

- [ ] **Step 6:** 执行记录填入本文件末尾（见下），提交：

```bash
git add docs/conventions.md README.md docs/plans/2026-07-12-doom-core-extraction.md
git commit -m "Document dream-hooks, logging, and defaults conventions"
```

---

## Verification（端到端验收）

1. `make check` — `Ran 39 tests, 39 results as expected`（29 基线 + 2 日志/错误 + 3 hook 机制 + 2 defaults + 3 宏）。
2. `make config-build && make check-isolated && make check-declare` — 两个新 core 文件被扫描收录且各自声明自足（gnutls 编译块、修饰键 Rule 5 存根）。
3. `grep -rn "on-first\|on-init-ui\|(require 'on)" --include='*.el' init.el core/ lisp/ lib/ test/ build/` — 零命中（site-lisp/on 子模块保留在原地，摘除由用户自行处理）。
4. `make smoke && make benchmark` — 可启动；p50/p95 相对基线无回归（init-ui 后移 + defaults 纯 setq 的实证）。
5. 交互冒烟 6 项 + 负向验证（T6 Step 4-5）。

## 风险与注意（执行时随手核对）

- **init-ui 时机后移**（after-init → window-setup）：若 GUI 冒烟发现 modeline/字体出现可感知的晚到，回退方案是 `dream-hooks-arm` 中非 daemon 分支改回 `after-init-hook`——一行改动，不影响其它机制。
- **first-buffer 的 *scratch* 谓词**：若用户习惯先在 scratch 打字再开文件，global-so-long-mode 推迟到首个真实 buffer——这正是想要的懒加载；但 benchmark 的 first-input 指标不含 so-long（它挂 first-buffer），对比口径不变。
- **`(symbol-value hook)` 检查与既有急加载测试**：`dream-config-registers-without-eagerly-loading-large-packages` let-bind dream-* hook 为 nil 后 require 各单元——batch 下 dream-hooks 不装配，测试只做 add-hook 注册断言，互不干扰。
- **lwarn 噪声**：T2 错误测试与负向验证会产生 Warning 输出，是行为的一部分，不是失败。
- **Windows 侧验证滞后**：修饰键/编码分支本次在 macOS 编译验证（w32 存根保证编译过），Windows 行为下次在该机器上冒烟。
- **on 子模块的后续摘除（用户自理）**：T3 之后自有代码零引用，随时可安全移除；届时聚合 autoloads 缓存跑一次 `make autoloads` 即不再含 on-autoloads（不摘除也无害，仅多注册一份无人调用的 autoloads）。
- **benchmark 基线**：若中途重建过 Emacs 或基线文件缺失，先 `make benchmark-baseline`（在 T0 基线代码上跑）再继续，保证对比有意义。

## 执行记录（执行时填写）

- T0 基线 `make benchmark` p50/p95：
- T6 终态 `make benchmark` p50/p95：
- `make check-isolated` 时长（新文件加入后）：
- 交互冒烟结论（macOS）：
- Windows 补验结论（后补）：
