# Dream Emacs 整体审查与修缮计划（2026-07-12）

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 对当前配置做整体审查后的修缮：修复 custom/状态文件卫生缺口、清理冲突与废弃设置、打开 Emacs 31 的 trusted-content、落地"lsp-mode ⇄ eglot 可切换"的新需求，以及（待你最终拍板的）flymake → flycheck 迁移。

**Architecture:** 全部改动是对既有结构的小修——状态文件重定向进 `.local/`（dream-paths 常量 + dream-defaults 设置，沿用 init-editing 的 setq-before-load 先例）；LSP 客户端切换用一个 `dream-lsp-client` defcustom + `dream-lsp` 分发函数，语言单元只改 hook 目标；诊断迁移替换 init-lang-programming 的 flymake 块并退役 lib/dream-flymake.el。零新机制，全部遵守既有编译期声明规范与两道检查门。

**Tech Stack:** Emacs 31.0.90 (emacs-plus), borg, setup.el/once.el, ERT, make。

## Global Constraints（每个任务都隐含遵守）

- **审查排除域（用户自理，本计划一行不碰）**：延迟加载、增量加载、hook 机制与挂接时机（dream-hooks/once/:iload/:once 的策略）、GC 策略、dream-runtime 的编译守卫与环境快照设计（仅修其中一处废弃变量）。
- **drone 子模块的增删一律由用户自行执行**——计划只列清单与前置条件，不跑 git submodule 命令。
- 全部自有文件严格编译（`byte-compile-error-on-warn t`）；每任务结束过 `make check`；改动运行时文件的任务追加 `make config-build && make check-isolated && make smoke`。
- 编译期声明按 conventions.md 决策链：新 setq 的非预加载 defcustom 一律进文件头 `cl-eval-when (compile)` 块。
- 提交用英语祈使句、显式 `git add <路径>`（不用 `-a`），结尾 `Co-Authored-By: Claude Fable 5 <noreply@anthropic.com>`。

---

## Context（审查结论）

### 基线健康度

- `make check` **67/67 全绿**（2026-07-12 实测，6.2s）。严格编译 + check-isolated/check-declare 两道门、autoloads 聚合缓存、manifest/trampoline 契约、benchmark 门——整体架构无大问题。
- 未提交 WIP 两处：init.el（仅补注释头）；init-completion.el 新增 `(setup corfu-history (:load-after corfu))`——与 corfu 的 `:once` 块内既有 `(require 'corfu-history)` 是重复的加载路径（T0 处理）。

### 发现的问题（已逐条实测核实）

| # | 级别 | 问题 | 证据 |
|---|---|---|---|
| 1 | 高 | `custom-file` 未设置（实测 nil）——误触 customize 会把表单追加进严格编译且 git 跟踪的 init.el，下次 `make config-build` 直接失败 | `emacs --batch` 实测 |
| 2 | 高 | 状态文件泄漏进仓库根：`projects.eld` 已出现（project-list-file 默认 `~/.emacs.d/projects.eld`）；同类未重定向：`nsm-settings-file`（network-security.eld）、`url-configuration-directory`（url/）、`multisession-directory`（multisession/） | git status `?? projects.eld`；各默认值 batch 实测 |
| 3 | 中 | `tab-always-indent` 冲突：init-editing.el 设 nil，init-completion.el corfu `:set` 设 `'complete`——加载顺序使 'complete 生效，nil 是死配置 | 两文件现行内容 |
| 4 | 中 | Emacs 31 `trusted-content` 未配置——untrusted elisp buffer 的宏展开分析/补全被限制（elisp-mode.el:299,862 实测），编辑自己的配置时 flymake/补全打折 | 31.0.90 源码 grep |
| 5 | 低 | early-init `idle-update-delay 1.0`：30.1 起废弃（现为 `which-func-update-delay` 别名），实际效果只是调慢 which-func，与降低重显负担的原意无关 | `byte-obsolete-variable` 实测 `(which-func-update-delay nil 30.1)` |
| 6 | 低 | `read-process-output-max` 双处设置：early-init 64KB → init-lsp 1MB（启动时被覆盖，early-init 值只活几秒） | 两文件现行内容 |
| 7 | 低 | dream-runtime.el `(set-default 'eshell-path-env path)`：`eshell-path-env` 自 29.1 废弃（`eshell-get-path`） | `byte-obsolete-variable` 实测 |
| 8 | 低 | early-init 无条件 `setq pgtk-wait-for-event-timeout`（mac/Windows 构建上凭空造变量）；init-ui 双重压制启动回显（变量 + advice 各一份） | 文件现行内容 |
| 9 | 低 | .gitignore 残留 `/epkgs/`（epkg 库早已迁到 .local/data/epkg） | .gitignore 现行内容 |

### 非问题（核实后排除）

- `c-ts-indent-offset`（init-lang-cpp）在 31 中真实存在（与 `c-ts-mode-indent-offset` 并存），非拼写错误。
- nerd-icons-corfu 已安装且带 autoload cookie，corfu-margin-formatters 引用安全。
- clutch 的 DB 后端 drone（chirp/mongodb/mysql/pg/redis）是真实依赖；cond-let/llama 是 magit 依赖；shrink-path/spinner/markdown-mode/yasnippet-snippets 均有宿主。

### 用户决策（已答复，直接作为任务输入）

1. **drone 库存**：约 18 个零引用 drone 全部保留，移除由用户自理；明确不用 flyover；**lsp-mode 与 eglot 都要保留且可切换**；诊断倾向 flycheck（flycheck 与第三方工具集成更好），eglot 诊断经 flycheck-eglot 桥接。
2. **custom-file**：指向 `.local/state/custom.el` 且启动**不加载**（配置保持纯声明式）。
3. **tab-always-indent**：保留 `'complete`，删除 init-editing 的 nil。
4. **jinx/Windows**：Windows 也要 jinx，用户自装 MSYS2/enchant 工具链；只补环境要求文档。

### 需用户后续自理的清单（登记，不入任务）

- drone 移除（含明确不用的 flyover；若 T5 落地 flycheck，flymake-clippy 也可移除）。
- T5 前置：assimilate `flycheck-eglot` drone（现未安装）。
- Windows 侧：MSYS2 + enchant（jinx）、Symbols Nerd Font Mono 字体安装（doom-modeline/nerd-icons-corfu 图标依赖）。

---

## 阶段总览

| 阶段 | 任务 | 目标 | 验收 |
|---|---|---|---|
| 0 · 基线 | T0 | WIP 落账，工作树干净 | `make check` 67/67；无未提交改动 |
| 1 · 文件卫生 | T1 | custom-file + 4 个状态文件重定向 + gitignore 清理 | ERT 67→68；仓库根无新增杂物 |
| 2 · 冲突与废弃 | T2 | 问题 3/5/6/7/8 清零 | ERT 68 全绿；grep 复核 |
| 3 · trusted-content | T3 | 配置树进入 31 的信任白名单 | ERT 68→69 |
| 4 · LSP 客户端切换 | T4 | `dream-lsp-client` defcustom + 分发函数，eglot 基线配置 | ERT 69→70；两客户端可 setopt 切换 |
| 5 · flycheck 迁移 | T5 | flymake → flycheck（**执行前需你口头最终确认**） | ERT 70 全绿（-1 +1）；.rs/.el 诊断经 flycheck |
| 6 · 文档 | T6 | conventions.md 补章 | 全量门绿 |

---

# 阶段 0 · 基线

## Task 0: WIP 落账

**Files:** Modify: `lisp/init-completion.el`；Commit: `init.el`

- [ ] **Step 1:** init-completion.el 删除新增的重复加载块（`:once (list :packages 'corfu)` 块内已有 `(require 'corfu-history)` + `(corfu-history-mode 1)`，`:load-after` 只多做一次裸 require）：

```elisp
(setup corfu-history
  (:load-after corfu))
```

整块删除。若你新增它是为了替换 :once 块内的手动 require（重构中间态），改为保留此块、从 :once 块删 `(require 'corfu-history)`——**执行时按你答复取其一**，默认删 `:load-after` 块。

- [ ] **Step 2:** `make check` → `Ran 67 tests, 67 results as expected`。
- [ ] **Step 3: Commit** —

```bash
git add init.el lisp/init-completion.el
git commit -m "Tidy init commentary and corfu-history loading"
```

---

# 阶段 1 · 文件卫生

## Task 1: custom-file 与状态文件重定向

**目标：** 问题 1/2/9。所有 vanilla Emacs 状态文件收进 `.local/`；custom-file 指向 state 且不加载；迁走已泄漏的 projects.eld；清理 gitignore 残留。

**Files:**
- Modify: `core/dream-paths.el`（常量区）、`core/dream-defaults.el`（新节 + 编译块）、`.gitignore`
- Move: `projects.eld` → `.local/state/projects.eld`
- Modify: `test/dream-core-test.el`（+1 测试）

**Interfaces:**
- Produces: `dream-custom-file`、`dream-project-list-file`、`dream-nsm-file`、`dream-url-directory`、`dream-multisession-directory` 常量（dream-paths）。

- [ ] **Step 1: 写失败测试** — test/dream-core-test.el 末尾追加：

```elisp
(ert-deftest dream-defaults-redirect-state-files-into-local ()
  (require 'dream-defaults)
  (dolist (value (list custom-file project-list-file nsm-settings-file
                       url-configuration-directory multisession-directory))
    (should (stringp value))
    (should (string-prefix-p dream-local-directory
                             (expand-file-name value)))))
```

- [ ] **Step 2:** `make check` → `Ran 68 tests, 67 results as expected, 1 unexpected`（失败于 custom-file nil）。
- [ ] **Step 3:** `core/dream-paths.el` 在 `dream-benchmark-directory` 定义之后追加：

```elisp
(defconst dream-custom-file (expand-file-name "custom.el" dream-state-directory))
(defconst dream-project-list-file
  (expand-file-name "projects.eld" dream-state-directory))
(defconst dream-nsm-file
  (expand-file-name "network-security.eld" dream-state-directory))
(defconst dream-url-directory
  (file-name-as-directory (expand-file-name "url" dream-cache-directory)))
(defconst dream-multisession-directory
  (file-name-as-directory
   (expand-file-name "multisession" dream-state-directory)))
```

（目录由各包按需创建，不加入 `dream-paths-initialize`。）

- [ ] **Step 4:** `core/dream-defaults.el`：
  - 文件头 `(require 'dream-hooks)` 之后补 `(require 'dream-paths)`（Rule 1，本文件现在直接引用其常量）。
  - 编译块扩为：

    ```elisp
    (cl-eval-when (compile)
      ;; `gnutls-verify-error' and friends are not preloaded.
      (require 'gnutls)
      ;; State-file defcustoms live in their packages, none preloaded.
      (require 'project)
      (require 'nsm)
      (require 'url)
      (require 'multisession))
    ```

  - `;;; Files` 节之后插入新节（setq-before-load：defcustom 在包加载时尊重既有值——与 init-editing 的 transient/bookmark 先例同机制）：

    ```elisp
    ;;; State files

    ;; Keep every state file out of the repository root, and never let
    ;; Customize write into the strictly compiled init.el.  The custom
    ;; file is deliberately not loaded: configuration stays declarative.
    (setq custom-file dream-custom-file
          project-list-file dream-project-list-file
          nsm-settings-file dream-nsm-file
          url-configuration-directory dream-url-directory
          multisession-directory dream-multisession-directory)
    ```

- [ ] **Step 5:** 迁移已泄漏文件并清理 gitignore：

```bash
mv projects.eld .local/state/projects.eld
```

`.gitignore` 删除这两行（epkg 库已在 .local/data/epkg，且下一行注释也一并删）：

```
/epkgs/
```

- [ ] **Step 6:** `make check` → 68/68；`make config-build && make check-isolated && make smoke` 全绿；`git status --short` 中 `?? projects.eld` 消失。
- [ ] **Step 7: Commit** —

```bash
git add core/dream-paths.el core/dream-defaults.el .gitignore test/dream-core-test.el
git commit -m "Redirect custom and state files into the local tree"
```

---

# 阶段 2 · 冲突与废弃清理

## Task 2: 设置冲突与废弃变量清零

**目标：** 问题 3/5/6/7/8，六处小改。

**Files:** Modify: `lisp/init-editing.el`、`lisp/init-completion.el`、`early-init.el`、`lisp/init-lsp.el`、`lisp/init-ui.el`、`core/dream-runtime.el`

- [ ] **Step 1: tab-always-indent 归一** —
  - `lisp/init-editing.el` 的

    ```elisp
    (setq-default indent-tabs-mode nil
                  tab-width 4
                  tab-always-indent nil)
    ```

    改为

    ```elisp
    (setq-default indent-tabs-mode nil
                  tab-width 4
                  tab-always-indent 'complete)
    ```

  - `lisp/init-completion.el` corfu `:set` 中删除一行 `tab-always-indent 'complete`（编辑默认值归 init-editing，一处定义）。

- [ ] **Step 2: early-init.el** —
  - 大 setq 中删除 `idle-update-delay 1.0`（30.1 废弃）与 `pgtk-wait-for-event-timeout 0.001`。
  - `read-process-output-max (* 64 1024)` 改为 `(* 1024 1024)`。
  - 大 setq 之后补：

    ```elisp
    (when (boundp 'pgtk-wait-for-event-timeout)
      (setq pgtk-wait-for-event-timeout 0.001))
    ```

- [ ] **Step 3:** `lisp/init-lsp.el` 顶部 setq 删除一行 `read-process-output-max (* 1024 1024)`（early-init 已定，避免双源）。
- [ ] **Step 4:** `lisp/init-ui.el` 删除文件尾的双重压制（`inhibit-startup-echo-area-message user-login-name` 已在同文件生效，advice 冗余）：

```elisp
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))
```

- [ ] **Step 5: eshell-path-env 现代化** — `core/dream-runtime.el` 的 `dream-runtime-apply-environment-snapshot` 中：

```elisp
    (when-let* ((path (getenv "PATH")))
      (setq exec-path (append (parse-colon-path path) (list exec-directory)))
      (set-default 'eshell-path-env path))
```

先验证再改：`emacs -Q --batch --eval "(progn (require 'esh-util) ...)"` 查看 31 的 `eshell-get-path` 实现——若它优先读 `eshell-path-env-list`（29+ 行为），将 `set-default` 行替换为：

```elisp
      (when (boundp 'eshell-path-env-list)
        (setq-default eshell-path-env-list (parse-colon-path path)))
```

若 `eshell-get-path` 已直接回退 `exec-path`/`(getenv "PATH")`，则整行删除。两种结果都消除对废弃变量的写入。

- [ ] **Step 6:** 复核 + 四连门：

```bash
grep -rn 'idle-update-delay\|eshell-path-env\b\|tab-always-indent' \
  --include='*.el' early-init.el init.el core/ lisp/ lib/
# 期望：仅 init-editing.el 一处 tab-always-indent 'complete（及可能的 eshell-path-env-list）
make check && make config-build && make check-isolated && make smoke
```

- [ ] **Step 7: Commit** —

```bash
git add lisp/init-editing.el lisp/init-completion.el early-init.el \
        lisp/init-lsp.el lisp/init-ui.el core/dream-runtime.el
git commit -m "Resolve setting conflicts and drop obsolete variables"
```

---

# 阶段 3 · trusted-content

## Task 3: 配置树进入信任白名单

**目标：** 问题 4。Emacs 31 对 untrusted elisp buffer 限制宏展开分析（elisp-mode.el:862 起：本地变量补全禁用、flymake byte-compile 受限）。把配置仓库列入 `trusted-content`。

**Files:** Modify: `core/dream-defaults.el`、`test/dream-core-test.el`（+1 测试）

- [ ] **Step 1: 写失败测试** —

```elisp
(ert-deftest dream-defaults-trust-the-configuration-tree ()
  (require 'dream-defaults)
  (should (cl-some (lambda (entry)
                     (and (stringp entry)
                          (file-equal-p entry user-emacs-directory)))
                   trusted-content)))
```

- [ ] **Step 2:** `make check` → 69 tests, 1 unexpected。
- [ ] **Step 3:** `core/dream-defaults.el` 在 `;;; State files` 节之后追加：

```elisp
;;; Trusted content

;; Emacs 31 restricts macro-expansion-based analysis (completion of
;; local variables, elisp flymake) in untrusted elisp buffers.  Our own
;; configuration is trusted by definition.
(add-to-list 'trusted-content
             (file-name-as-directory (file-truename user-emacs-directory)))
```

（`trusted-content` 目录条目必须以 `/` 结尾且经 truename 比对——`file-name-as-directory` + `file-truename` 双保险。若日后想信任 `~/Workspace/`，同式样再加一行即可，本次不加。）

- [ ] **Step 4:** `make check` → 69/69；`make config-build && make check-isolated` 绿。
- [ ] **Step 5: Commit** —

```bash
git add core/dream-defaults.el test/dream-core-test.el
git commit -m "Trust the configuration tree for elisp analysis"
```

---

# 阶段 4 · LSP 客户端切换

## Task 4: dream-lsp-client 分发（lsp-mode ⇄ eglot）

**目标：** 用户新需求："lsp-mode 和 eglot 都需要保留，提供选项进行切换"。一个 defcustom + 纯函数映射 + 分发命令；四个语言单元的 hook 目标从 `lsp-deferred` 改为 `dream-lsp`。lsp-mode 现有配置不动；eglot 补基线配置与 consult-eglot 键位（对称于 consult-lsp）。

**Files:**
- Modify: `lisp/init-lsp.el`（defcustom + 分发 + eglot 块 + 编译块扩充）
- Modify: `lisp/lang/init-lang-cpp.el`、`init-lang-rust.el`、`init-lang-web.el`（hook 目标替换）
- Modify: `test/dream-core-test.el`（+1 新测试；2 个既有测试改断言）

**Interfaces:**
- Produces: `dream-lsp-client` defcustom（'lsp-mode | 'eglot，默认 'lsp-mode）；`(dream-lsp--client-function)` → 返回 `#'lsp-deferred` 或 `#'eglot-ensure`；`(dream-lsp)` interactive 分发命令——语言单元统一挂它。

- [ ] **Step 1: 写失败测试** — test/dream-core-test.el 末尾追加：

```elisp
(ert-deftest dream-lsp-dispatcher-maps-client-choice-to-function ()
  (require 'init-lsp)
  (let ((dream-lsp-client 'lsp-mode))
    (should (eq (dream-lsp--client-function) #'lsp-deferred)))
  (let ((dream-lsp-client 'eglot))
    (should (eq (dream-lsp--client-function) #'eglot-ensure)))
  (let ((dream-lsp-client 'unknown))
    (should-error (dream-lsp--client-function) :type 'user-error)))
```

- [ ] **Step 2:** `make check` → 70 tests, 1 unexpected（void-function dream-lsp--client-function）。
- [ ] **Step 3:** `lisp/init-lsp.el`：
  - 编译块扩为：

    ```elisp
    (cl-eval-when (compile)
      (require 'lsp-mode)
      ;; These two defcustoms live in their own files, not lsp-mode.el.
      (require 'lsp-diagnostics)
      (require 'lsp-completion)
      (require 'consult-lsp)
      (require 'eglot)
      (require 'consult-eglot))
    ```

  - 顶部 setq 块之前插入分发层：

    ```elisp
    (defcustom dream-lsp-client 'lsp-mode
      "Which LSP client `dream-lsp' starts in a buffer."
      :type '(choice (const :tag "lsp-mode" lsp-mode)
                     (const :tag "Eglot" eglot))
      :group 'dream)

    (defun dream-lsp--client-function ()
      "Return the entry-point function for `dream-lsp-client'."
      (pcase dream-lsp-client
        ('lsp-mode #'lsp-deferred)
        ('eglot #'eglot-ensure)
        (other (user-error "Unknown dream-lsp-client: %S" other))))

    (defun dream-lsp ()
      "Start the configured LSP client in the current buffer."
      (interactive)
      (funcall (dream-lsp--client-function)))
    ```

  - 文件尾（consult-lsp 块之后）补 eglot 基线（对称于 lsp-mode 侧的既有配置）：

    ```elisp
    (setup eglot
      (:set eglot-autoshutdown t
            eglot-events-buffer-config '(:size 0 :format full)
            eglot-extend-to-xref t)
      (:when-loaded
        (keymap-set eglot-mode-map
                    "<remap> <xref-find-apropos>" #'consult-eglot-symbols)
        ;; lsp-mode downloads the Vue server itself; Eglot needs the
        ;; command on PATH.  Adjust when the volar CLI changes.
        (add-to-list 'eglot-server-programs
                     '(web-mode . ("vue-language-server" "--stdio")))))
    ```

- [ ] **Step 4:** 语言单元 hook 目标替换（四处，均在 `once (list :packages 'init-lsp)` 块内）：
  - `init-lang-cpp.el`：`#'lsp-deferred` → `#'dream-lsp`（两行）。
  - `init-lang-rust.el`：同（一行）。
  - `init-lang-web.el`：同（一行）；`lsp-language-id-configuration` 的 once 块保留不动（仅 lsp-mode 消费它，eglot 走 server-programs）。
- [ ] **Step 5:** 既有测试改断言：
  - `dream-config-registers-without-eagerly-loading-large-packages`（:208）：`(should (memq #'lsp-deferred (symbol-value hook)))` → `#'dream-lsp`；并在 featurep 断言列表补 `eglot`（切换机制不得急加载任一客户端）。
  - `dream-language-lsp-integration-is-order-independent`（:278-288）：两处 `#'lsp-deferred` → `#'dream-lsp`。
- [ ] **Step 6:** `make check` → 70/70；`make config-build && make check-isolated && make smoke` 绿。
- [ ] **Step 7: 交互冒烟** — 打开 .rs：默认 lsp-mode 挂起；`(setopt dream-lsp-client 'eglot)` 后 `M-x dream-lsp` 于新 buffer 启动 eglot；`xref-find-apropos` remap 在两客户端下各自映射 consult 命令。
- [ ] **Step 8: Commit** —

```bash
git add lisp/init-lsp.el lisp/lang/init-lang-cpp.el lisp/lang/init-lang-rust.el \
        lisp/lang/init-lang-web.el test/dream-core-test.el
git commit -m "Add switchable LSP client dispatch for lsp-mode and eglot"
```

---

# 阶段 5 · flycheck 迁移（执行前需最终确认）

## Task 5: flymake → flycheck

**前置（用户自理，缺一不启动本任务）：** (a) 口头确认"最终选 flycheck"；(b) assimilate `flycheck-eglot` drone。flycheck 与 consult-flycheck 已安装。完成后 `flymake-clippy` drone 可由你移除（flycheck 内置 rust-clippy checker）。

**目标：** init-lang-programming 的 flymake 块整体替换；lib/dream-flymake.el 退役（错误导航由 `flycheck-list-errors`/consult-flycheck 承担）；lsp-mode 诊断切到 :flycheck；eglot 诊断经 flycheck-eglot。

**Files:**
- Modify: `lisp/lang/init-lang-programming.el`、`lisp/lang/init-lang-rust.el`、`lisp/init-lsp.el`
- Delete: `lib/dream-flymake.el`
- Modify: `test/dream-core-test.el`（删 1 改 2 增 1）

- [ ] **Step 1: 写失败测试** —

```elisp
(ert-deftest dream-lang-programming-registers-flycheck ()
  (require 'init-lang-programming)
  (should (memq 'flycheck-mode prog-mode-hook))
  (should-not (featurep 'flycheck)))
```

- [ ] **Step 2:** `make check` → 1 unexpected（flymake-mode 仍在 prog-mode-hook，flycheck-mode 不在）。
- [ ] **Step 3:** `lisp/lang/init-lang-programming.el`：
  - 编译块 `(require 'flymake)` → `(require 'flycheck)`。
  - 删除 `dream-programming--with-elisp-load-path` 函数与整个 `(setup flymake ...)` 块，替换为：

    ```elisp
    (setup flycheck
      (:hook-into prog-mode)
      (:global "C-c f" flycheck-list-errors)
      (:set flycheck-emacs-lisp-load-path 'inherit
            flycheck-display-errors-delay 0.25
            flycheck-check-syntax-automatically '(save mode-enabled)))
    ```

    （`flycheck-emacs-lisp-load-path 'inherit` 取代原 elisp-flymake 的 load-path advice——flycheck 的 elisp checker 在子进程跑，天然绕开 dream-runtime 编译守卫。）
- [ ] **Step 4:** `lisp/lang/init-lang-rust.el`：删除编译块与整个 `(setup flymake-clippy ...)` 块（flycheck 内置 rust-clippy checker，随 rust-ts-mode 自动可用）。文件仅剩 dream-lsp hook。
- [ ] **Step 5:** `lisp/init-lsp.el`：
  - `lsp-diagnostics-provider :flymake` → `:flycheck`。
  - eglot 块 `:when-loaded` 内追加桥接：

    ```elisp
    (when (require 'flycheck-eglot nil t)
      (global-flycheck-eglot-mode 1))
    ```

    编译块补 `(require 'flycheck-eglot)`。
- [ ] **Step 6:** 退役 flymake 扩展：`git rm lib/dream-flymake.el`；删除测试 `dream-flymake-error-row-boundary-matches-emacs-31-metadata`（:343）；改写 `dream-extensions-load-at-declared-boundaries`（:329）——去掉 dream-flymake/flymake 断言，只保留 eldoc 边界部分。
- [ ] **Step 7:** `make check` → `Ran 70 tests, 70 results as expected`（-1 flymake 行测试 +1 flycheck 注册测试）。`make config-build && make check-isolated && make check-declare && make smoke` 全绿。
- [ ] **Step 8: 交互冒烟** — .el 文件保存触发 flycheck（emacs-lisp checker 用完整 load-path）；.rs 在 lsp-mode 与 eglot 两种客户端下都有诊断（lsp→flycheck 原生；eglot→flycheck-eglot）；`C-c f` 打开错误列表。
- [ ] **Step 9: Commit** —

```bash
git add lisp/lang/init-lang-programming.el lisp/lang/init-lang-rust.el \
        lisp/init-lsp.el test/dream-core-test.el
git commit -m "Migrate diagnostics from flymake to flycheck"
```

（`git rm` 的 lib/dream-flymake.el 删除已在暂存区，随本提交入库。）

---

# 阶段 6 · 文档

## Task 6: conventions.md 补章

**Files:** Modify: `docs/conventions.md`

- [ ] **Step 1:** 在"## 构建与产物"章之前插入新章：

```markdown
## 状态文件与 customize

- 一切运行期状态文件必须落在 `.local/` 下（常量定义于 core/dream-paths.el，
  设置集中在 core/dream-defaults.el 的 State files 节与 init-editing.el）。
  仓库根出现新的未跟踪文件即视为泄漏：找到对应变量重定向，勿加 gitignore。
- `custom-file` 指向 `.local/state/custom.el` 且**不加载**——配置是纯声明式
  的，customize 写入是无害弃置。需要持久化的选项写进对应 init 单元。
- Emacs 31 的 `trusted-content` 已包含本配置树；新增可信目录在
  dream-defaults.el 的 Trusted content 节按同式样追加。

## LSP 客户端与诊断

- LSP 客户端经 `dream-lsp-client`（'lsp-mode | 'eglot）切换；语言单元一律
  挂 `dream-lsp`，不得直接挂 `lsp-deferred`/`eglot-ensure`。
- 诊断统一走 flycheck（lsp-mode 用 :flycheck provider；eglot 经
  flycheck-eglot 桥接）。<!-- T5 未执行则删除本条 -->

## Windows 环境要求

- jinx：需要 MSYS2 工具链与 enchant 库（首次启用时本机编译 C 模块）。
- 图标：Symbols Nerd Font Mono 需手动安装（doom-modeline / nerd-icons-corfu）。
```

- [ ] **Step 2:** 全量门：

```bash
make check && make config-build && make check-isolated && make check-declare \
  && make smoke && make benchmark
```

- [ ] **Step 3: Commit** —

```bash
git add docs/conventions.md
git commit -m "Document state-file, LSP client, and Windows conventions"
```

---

## Verification（端到端验收）

1. `make check` 全绿（T4 后 70；T5 执行后仍 70：-1 flymake +1 flycheck）。
2. `make config-build && make check-isolated && make check-declare && make smoke && make benchmark` 全绿，benchmark 无回归（本计划全部是 setq/小函数，理论零启动影响）。
3. 卫生复核：`git status --short` 仓库根无未跟踪状态文件；`emacs -Q --batch -l early-init.el -l init.el --eval '(princ custom-file)'` 输出落在 .local/state 下。
4. 交互冒烟：M-x customize-variable 任改一项保存 → init.el 无变化、.local/state/custom.el 出现表单；编辑 core/*.el 时 elisp 补全/分析不再报 untrusted；`setopt dream-lsp-client 'eglot` 后新 buffer 走 eglot。
5. T5 后：.el/.rs 诊断经 flycheck，两客户端诊断均可见。

## 风险与注意

- **project-list-file 等 setq-before-load**：依赖 defcustom 尊重既有值的标准机制（与 transient/bookmark 先例相同）；隔离编译门会强制编译块里的 require 齐全。
- **projects.eld 迁移**：迁移前该文件内容是仓库根版本；如你已在别处有更新的 project 列表，以实际为准（文件是可再生缓存，丢了也无害）。
- **eglot + Vue**：`vue-language-server` 命令需在 PATH（lsp-mode 是自动下载的）；volar 新版可能需要 initializationOptions——T4 只保证机制可切换，Vue on eglot 的服务器调优留待实际使用时补。
- **T5 的既有测试面**：eager-load 测试的 featurep 断言列表在 T4/T5 各扩一次（eglot、flycheck 不得急加载）；若发现 flycheck 被某路径提前拉起，隔离检查与该测试会点名。
- **flymake-clippy/flyover/其余零引用 drone**：移除是你的操作；T5 之后 flymake-clippy 才安全可移。
- **Windows 复验滞后**：jinx 工具链与字体安装完成前，Windows 侧首次启用 jinx 会报模块编译错误——这是你选定"Windows 也要 jinx"的已知代价，文档已写明。

## 执行记录（执行时填写）

- T0 前 `git status` 摘要：
- T6 后 `make benchmark` p50/p95 对比：
- flycheck 最终确认（是/否，日期）：
- Windows 复验结论（后补）：
