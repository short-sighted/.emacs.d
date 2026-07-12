# Dream 配置组织约定

## 目录职责

- `core/` 只放启动必需基础设施，文件名为 `dream-*.el`。
- `lib/` 放可独立加载、可测试的扩展库，文件名为 `dream-*.el`。
- `lisp/` 放领域配置，顶层入口为 `init-*.el`；语言配置放在 `lisp/lang/`。
- `build/` 是构建期代码，`test/` 是 ERT、smoke 和 benchmark，`site-lisp/` 由 Borg 管理。

所有库 basename 全局唯一，文件头启用 lexical binding，文件尾的 `provide` 必须与 basename
一致。合集文件只装配同一领域，例如 `init-lang.el`；真正跨领域的行为应成为独立单元，
由条件协调，而不是让两个合集互相 `require`。

## 扩展挂载

扩展一个尚未预加载的包时，在该包的 setup 中用 `:also-load`：

```elisp
(setup flymake
  (:also-load dream-flymake))
```

扩展 Emacs 已可能预加载的包时，在真实启用 hook 上用 `:require-once`，避免等待一个已经发生的
feature load 事件：

```elisp
(setup eldoc
  (:require-once (list :hooks 'eldoc-mode-hook) 'dream-eldoc))
```

反例是无条件在启动期加载 `dream-flymake`，或只用 `with-eval-after-load` 挂载
`dream-eldoc`。前者扩大启动集，后者在 feature 已加载时不能表达首次实际使用的边界。

## Hook 与 once

- 普通 mode hook 表达逐 buffer 行为。Rust 每个 buffer 都要启用检查，因此使用
  `(:with-hook rust-ts-mode-hook (:hook flymake-clippy-setup))`，不能用 `once` 替代。
- `dream-first-input-hook`、`dream-first-file-hook`、`dream-first-buffer-hook` 与
  `dream-init-ui-hook` 表达一次性的编辑器生命周期事件；它们不是包依赖协议。
- `once` 协调可能以任意顺序到达的 feature、package、hook 或 advice 条件，并保证动作只安装一次。
  C++/Rust/Web 与 `init-lsp` 的集成使用 package 条件，既支持先加载语言配置，也支持先加载 LSP。
- `:require-once` 是首次满足条件时 require 一个库；`:once` 执行任意一次性表单；`:iload`
  把 feature 按优先级加入空闲增量队列。

不要用一次性机制承载每个 buffer 都必须重复执行的初始化，也不要用普通 hook 模拟多个 feature
的顺序无关汇合。强制运行期依赖直接 `require`；判断当前状态用 `featurep`；未来才可能到达、
且顺序不确定的状态才交给 `once`。

## Load Path

`init.el` 只手工加入 `core/`，随后 `dream-paths-add-load-paths` 按 Emacs 的标准子目录规则加入
`lib/` 和 `lisp/`。隐藏目录、非字母数字开头目录、RCS/CVS 和带 `.nosearch` 的树不参与搜索。
重复调用必须幂等，Borg 改变文件布局后会清理 Emacs 31 的 load-path inventory cache。

`early-init.el` 将 `user-lisp-auto-scrape` 设为 `nil`，避免 Emacs 31 的 User Lisp Directory
自动扫描与显式布局叠加，造成顺序漂移或 shadow。这里访问私有 cache 变量被限制在
`dream-paths-reset-load-path-cache` 一个边界内，直到 Emacs 提供公开失效 API。

## 构建与产物

构建器显式列出 `core/`，扫描 `lib/**/dream-*.el` 和 `lisp/**/init-*.el`，最后编译 `init.el`；
扫描与运行时遵守同一 `.nosearch` 规则，并在编译前拒绝重复 basename。autoload 也从同一 owned
tree 生成，避免手工文件清单漂移。

`make config-build` 是默认且可逆的 byte 策略：它删除当前 owned source 对应的 ELN，但绝不清理
第三方 ELN。`make config-native` 是显式实验入口。manifest 记录 Emacs/ABI、source hash、
`:config-native` 与 `:packages-native`；它描述本次产物，不替代磁盘验证。Borg 的 `quick`/`native`
完成包构建后调用同一配置构建入口，默认由 `dream-build-default-config-native` 决定。

## setup 关键字

自有关键字有八个：

- `:hooks`：按 `HOOK FUNCTION` 对批量挂接具名函数。
- `:load-after`：等待多个 feature 依次加载后 require 当前 feature。
- `:after`：当前 feature 加载后执行表单。
- `:global`：全局按键，如 Flymake 诊断入口；反例是用它改 mode map。
- `:set`：注册阶段设置变量，如 `jinx-languages`；包必须加载后才存在的 setter 应用 `:when-loaded`。
- `:autoload`：为当前 feature 声明命令 autoload，如 `borg-build`；反例是 autoload 普通非交互函数。
- `:advice`：feature 加载后安装 advice，如 Borg 构建后的 cache 失效；反例是 advice 尚未定义且不属于当前 feature 的符号。
- `:needs`：缺少外部程序时退出当前 setup，如 Rust 的 `cargo`；反例是把可选能力写成全局启动错误。

常用上游关键字保留原义：`:when-loaded` 放加载后配置，`:with-hook`/`:hook` 放重复 hook 行为，
`:also-load` 加载配套 feature。`once-setup` 提供 `:once`、`:require-once` 与 `:iload`。
不要再引入 `:init`、`:opt` 或 `:bind-map` 等额外别名。

## 编译期声明规范

自有配置使用 `byte-compile-error-on-warn t` 严格编译。原则是让编译器看见真定义，而不是
用存根让它沉默。顶层 `require` 同时在编译、加载和解释源码时执行，用于运行期真依赖；
`eval-and-compile` 仅用于 `init.el` load-path 和 `dream-setup.el` 别名引导；
`eval-when-compile` 适用于解释源码时也要展开的宏提供者，当前没有使用点。

仅编译期需要外部符号知识时，文件头使用：

```elisp
(cl-eval-when (compile)
  (require 'package-feature))
```

该表单不写入 `.elc`，解释执行源码时也不加载 package，因此不改变运行期惰性语义。

### 编译环境契约

配置在 `borg-initialize` 后编译：全部 drone 已进入 load-path，聚合 autoload 已注册，且
`LSP_USE_PLISTS=true`。带 autoload cookie 的函数无需声明；变量不由 autoload 提供；
Emacs 预加载符号无需处理。native 编译复用 byte compiler 前端，同一声明同时治理 ELC/ELN。

决策顺序如下：

1. Emacs 已预加载的符号不写声明。
2. 运行期真依赖使用顶层 `require`。
3. 仅编译期符号知识使用一个 `cl-eval-when (compile)` 块。
4. 编译期和运行期都需要的引导代码才使用 `eval-and-compile`。
5. 只有定义处不 provide feature 或包不在 load-path 时才允许残余 `defvar`/`declare-function`；后者必须填写 FILE，并注释原因。

禁止用可 require 包的存根压制警告。旧的 `lsp-keep-workspace` 存根曾掩盖真实变量名
`lsp-keep-workspace-alive`；Clippy 的函数声明曾掩盖它没有 autoload cookie；C tree-sitter
存根还掩盖了 Emacs 31 将缩进选项改名为 `c-ts-indent-offset`。这些都是编译器依赖必须指向
真实定义的直接理由。

`make config-build` 为快速共享会话构建，可能被更早文件加载的 feature 掩蔽；
`make check-isolated` 在全新 Emacs 子进程逐文件严格编译，是启用 ELC 后的提交前硬门；
`make check-declare` 校验残余函数声明。当前隔离门约 `5.83s`，不并入日常 `make check`。

## 启动钩子与日志规范

### 一次性启动钩子（core/dream-hooks.el）

按启用时机选择挂接点，全部由 `dream-run-hook-on` 驱动：成功触发后
hook 变量清零；错误升格为携带 hook 名与函数名的 `dream-hook-error`，
失败时保留 hook 变量供诊断，但触发链会熄火，不会重复报错。

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
