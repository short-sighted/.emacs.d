# Dream 配置组织约定

## 目录与命名

- `core/` 只放启动必需基础设施，文件名为 `dream-*.el`。
- `lib/` 放可独立加载、可测试的扩展库，文件名为 `dream-*.el`。
- `lisp/` 放领域配置，顶层入口为 `init-*.el`；语言配置放在 `lisp/lang/`。
- `build/` 只放显式构建代码；`test/` 放 ERT、smoke 和 benchmark；`site-lisp/` 由 Borg 管理。

所有库 basename 全局唯一，文件头启用 lexical binding，文件尾 `provide` 必须与 basename
一致。合集文件只装配同一领域，例如 `init-lang.el`；跨领域行为应成为独立单元，由条件协调，
不能让两个合集互相 `require`。

自有符号统一使用以下形式：

- 公共 API：`dream-NAME` 或 `dream-SUBSYSTEM-NAME`。
- 私有实现：`dream-SUBSYSTEM--NAME`。
- error、hook、defcustom 和 feature 同样遵守该前缀。
- 不使用 Doom 风格的 `!` 后缀、`dream/command` slash 命名或无期限兼容 alias。

基础 API 唯一入口是 `core/dream-core.el` / feature `dream-core`。旧 `dream-lib` 名称不保留
兼容层，避免源码、ELC 和 feature 三套名字并存。

## 装配与 Load Path

`init.el` 只手工加入 `core/`，随后 `dream-paths-add-load-paths` 按 Emacs 标准子目录规则加入
`lib/` 和 `lisp/`。隐藏目录、非字母数字开头目录、RCS/CVS 和带 `.nosearch` 的树不参与搜索。
重复调用必须幂等，Borg 改变文件布局后清理 Emacs 31 的 load-path inventory cache。

`early-init.el` 将 `user-lisp-auto-scrape` 设为 nil，避免 Emacs 31 的 User Lisp Directory 自动
扫描与显式布局叠加。访问私有 `load-path-filter--cache` 只允许出现在
`dream-paths-reset-load-path-cache` 这一处，直到 Emacs 提供公开失效 API。

## 扩展、Hook 与 once

扩展一个尚未预加载的包时，在该包 setup 中用 `:also-load`：

```elisp
(setup flymake
  (:also-load dream-flymake))
```

扩展 Emacs 可能已经预加载的包时，在真实启用 hook 上用 `:require-once`，避免等待一个已经发生
的 feature load 事件：

```elisp
(setup eldoc
  (:require-once (list :hooks 'eldoc-mode-hook) 'dream-eldoc))
```

选择机制时遵守以下边界：

- 普通 mode hook 表达逐 buffer 行为。Rust 每个 buffer 都要启用检查，因此必须使用真实
  `rust-ts-mode-hook`，不能用 once 替代。
- `dream-first-input-hook`、`dream-first-file-hook`、`dream-first-buffer-hook` 与
  `dream-init-ui-hook` 表达一次性的编辑器生命周期，不是包依赖协议。
- `once` 协调可能以任意顺序到达的 feature、package、hook 或 advice 条件，并保证动作只安装一次。
- `:require-once` 首次满足条件时 require 一个库；`:once` 执行任意一次性表单；`:iload` 把
  feature 按深度加入空闲增量队列。

不要用 once 承载每个 buffer 都要重复执行的初始化，也不要用普通 hook 模拟多个 feature 的
顺序无关汇合。强制运行期依赖直接 `require`；当前状态用 `featurep`；未来才可能到达且顺序
不确定的状态才交给 once。

### 一次性生命周期

`dream-run-hook-on` 为一个生命周期注册 hook、advice 和 daemon frame 等全部触发器。任一触发器
进入执行后，无论成功、predicate 报错还是 hook 函数报错，都会拆除整组 sibling trigger；
`dream-hooks-disarm` 可在触发前显式完成相同清理。

`dream-run-hooks` 会继续执行每个 hook 中的全部函数，也会在一个 hook 失败后继续执行参数中的
后续 hook。单个 hook 失败时保持直接结构：

```elisp
(dream-hook-error
 :hook HOOK
 :failures ((FUNCTION . ERROR) ...))
```

多个 hook 同时失败时，`:hook` 是 hook 列表，`:failures` 按 hook 分组。这样单链调用者仍能直接
定位函数，而 daemon dispatcher 可以在所有生命周期链都尝试完成后统一报告。

清理完成后才 signal。成功时生命周期 hook 变量清零；失败时保留函数列表用于诊断，但注册已经
disarm，不会在下一次输入或 frame 上重复报错。daemon 的共享 dispatcher 按注册顺序尝试全部
生命周期链，一条链失败不会阻止字体、UI 等后续链；最后再聚合错误。first-buffer 的全局 window
trigger 检查触发参数里的实际 frame/window buffer，而不是错误地依赖当时的 current buffer。

| Hook | 触发时机 | 适用 |
| --- | --- | --- |
| `dream-first-input-hook` | 首次用户输入前 | 补全和输入相关全局模式 |
| `dream-first-file-hook` | 首次交互式打开文件前 | recentf、auto-revert |
| `dream-first-buffer-hook` | 首个真实 buffer，排除 `*scratch*` | so-long 等 buffer 基础设施 |
| `dream-init-ui-hook` | window setup 或 daemon 首帧 | modeline、字体和 frame mode |

## setup 关键字

自有关键字保持八个：

- `:hooks`：按 `HOOK FUNCTION` 对批量挂接具名函数。
- `:load-after`：等待多个 feature 依次加载后 require 当前 feature。
- `:after`：当前 feature 加载后执行表单。
- `:global`：全局按键；不能用它修改 mode map。
- `:set`：注册阶段设置变量；包加载后才存在的 setter 应用 `:when-loaded`。
- `:autoload`：为当前 feature 声明交互命令 autoload。
- `:advice`：feature 加载后安装已经定义且属于该 feature 的 advice。
- `:needs`：缺少外部程序时退出当前 setup。

上游 `:when-loaded`、`:with-hook`、`:hook`、`:also-load` 保留原义；`once-setup` 提供
`:once`、`:require-once` 与 `:iload`。不要增加只为隐藏编译期依赖的 setup 关键字，也不要
重新引入 `:init`、`:opt` 或 `:bind-map` 等别名。

## 构建与产物

构建器显式列出 `core/`，扫描 `lib/**/dream-*.el` 和 `lisp/**/init-*.el`，最后编译
`init.el`。扫描与运行时遵守同一 `.nosearch` 规则，编译前拒绝重复 basename。autoload 从
同一 owned tree 生成，避免手工文件清单漂移。`early-init.el` 不编译，但属于 manifest 的启动
输入；`dream-*.elc`/`init-*.elc` 失去对应源码时由完整配置构建清除。

`make config-build` 是默认且可逆的 byte 策略：只删除当前 owned source 对应的 ELN，绝不清理
第三方 ELN。`make config-native` 是同步、显式的实验入口。Borg 的 `quick`/`native` 在包构建后
进入同一配置构建入口，默认由 `dream-build-default-config-native` 决定。单独构建一个 drone 只
刷新聚合 autoload；没有重建 owned ELC 时不得重写配置 manifest。

manifest schema 2 记录 Emacs 版本、`system-configuration`、native ABI、source hash、ELN 根、
LSP plist ABI、环境 identity、所需 primitive trampoline、`:config-native` 和
`:packages-native`。兼容检查同时验证 source hash 与 trampoline 文件确实存在，manifest 不能
代替磁盘事实。

因为 `load-prefer-newer=nil`，源码比 ELC 新时仍会继续使用旧 ELC，并在延迟检查中警告。这个
选择避免每次 require 做 mtime 策略切换，但也形成明确责任：修改 owned source 后必须运行
`make config-build`。`make runtime-no-compile` 要求 manifest 与源码完全一致，否则拒绝测试。

### 运行期零编译契约

`dream-runtime-initialize` 在普通会话中执行以下策略：

1. `native-comp-jit-compilation=nil`。
2. `native-comp-enable-subr-trampolines` 指向当前 ABI 的预建目录，而不是简单设 nil；后者会破坏
   primitive advice 的语义。
3. 守卫 `byte-compile-file`、`native-compile`、`native-compile-async` 和
   `comp-trampoline-compile`。没有动态许可就 signal `dream-runtime-compilation-error`。
4. 缺少产物时 fail closed，不在首次使用时编译。

若 Emacs 31 构建本身没有 native compiler，native ABI 记为 `"none"`，trampoline contract 为空；
byte-only 配置仍可构建，且 ELC compiler guard 继续生效。

`dream-with-runtime-compilation` 只允许出现在明确的构建边界。`build/dream-compile.el`、用户主动
执行的 Borg build，以及 Elisp Flymake 临时诊断在各自最小动态范围内获得许可。外部语言编译器、
tree-sitter grammar 安装和非 Lisp 进程不受该持久产物策略影响。

`make runtime-artifacts` 预建当前 ABI 所需 trampoline，并在 macOS 上调用 login shell 生成环境
快照。`make config-build` 和 `make config-native` 都会先调用它。正常 GUI/daemon 启动只把权限
`0600` 的 inert data 应用到 `process-environment`、`exec-path` 和 `eshell-path-env`；若 identity
不兼容、变量表缺失或变量项不是 `(STRING . STRING-OR-NIL)`，则只警告。用户可明确运行
`dream-environment-refresh` 刷新，禁止在 frame hook 中同步启动 shell。

`make runtime-no-compile` 在启动、startup hook、首次 input/file/buffer、Elisp buffer 与全部 idle
任务前后比较配置树内每个 ELC/ELN 的路径、大小和 mtime，并检查 native async queue 为空及全部
compiler guard 仍然安装。

## 编译期声明规范

自有配置使用 `byte-compile-error-on-warn t` 严格编译。原则是让编译器看见真定义，而不是用存根
让它沉默。顶层 `require` 同时在编译、加载和解释源码时执行，用于运行期真依赖；
`eval-and-compile` 仅用于 `init.el` load-path 和 `dream-setup.el` 别名引导。

仅编译期需要外部符号知识时，文件头使用：

```elisp
(cl-eval-when (compile)
  (require 'package-feature))
```

该表单不写入 ELC，解释执行源码时也不加载 package，因此不改变运行期惰性语义。

### 编译环境契约

配置在 `borg-initialize` 后编译：全部 drone 已进入 load-path，聚合 autoload 已注册，且
`LSP_USE_PLISTS=true`。带 autoload cookie 的函数无需声明；变量不由 autoload 提供；Emacs
预加载符号无需处理。native 编译复用 byte compiler 前端，同一声明同时治理 ELC/ELN。

决策顺序如下：

1. Emacs 已预加载的符号不写声明。
2. 运行期真依赖使用顶层 `require`。
3. 仅编译期符号知识使用一个 `cl-eval-when (compile)` 块。
4. 编译期和运行期都需要的引导代码才使用 `eval-and-compile`。
5. 只有定义处不 provide feature 或包不在 load-path 时才允许残余 `defvar`/`declare-function`；
   `declare-function` 必须填写 FILE 并注释原因。

禁止用可 require 包的存根压制警告。共享 `make config-build` 会话可能被前面文件已加载的 feature
掩蔽；`make check-isolated` 在全新 Emacs 子进程逐文件严格编译，`make check-declare` 校验残余
函数声明。三者共同构成启用 ELC 后的提交前硬门。

## 字体规范

字体职责只属于 `core/dream-fonts.el`；`init-ui.el` 只启用 modeline 和 frame-wide mode。公共
defcustom 分为 primary、variable pitch、serif、symbol、emoji、CJK 六个 role，另有 point size、
step 和 CJK scale。nil 表示自动 fallback，variable/serif 的 nil 表示继承 primary。

使用 Emacs 31 的 `font-family-list FRAME` 按 display 探测字体。显式配置缺失 signal
`dream-font-error`；自动 fallback 全部缺失只写 debug log。face 应用以 weak frame hash 去重，
fontset 同样逐 frame 覆盖，CJK 的 `face-font-rescale-alist` 原值必须在 reload 前恢复。symbol 同时
覆盖 `symbol`/`mathematical` script，emoji 覆盖 emoji script 并追加 symbol fallback，Nerd Font
覆盖两个 Private Use Area。不得向全局 fontset 重复 prepend fallback。

`dream-font-reload` 是唯一主动清 Emacs font cache 并重放全部 GUI frame 的入口；
`dream-font-increase`、`dream-font-decrease`、`dream-font-reset` 只修改全局 point size 后调用 reload。
尺寸调整若 reload 失败，必须回滚 point size 和初始尺寸状态并重放旧设置。不提供 big-font mode。
终端调用必须 no-op。`after-make-frame-functions` 覆盖所有新 GUI frame，不只 server frame；自定义
face 可挂 `dream-after-setting-font-hook`。

## 日志、错误与基础 API

`(dream-log [LEVEL] FORMAT ARGS...)` 的 LEVEL 缺省 2；日志关闭时参数不求值。交互会话默认静默；
`emacs --debug-init` 打开级别 2，`DEBUG=3 emacs --debug-init` 打开调试级；batch 恒为级别 3。
输出落入 `*Messages*`，带相对 `before-init-time` 的时间戳。

自有错误都继承 `dream-error`。现有子类为 `dream-hook-error`、
`dream-runtime-compilation-error` 和 `dream-font-error`。

`core/dream-core.el` 提供以下基础工具：

- `dream-add-hook`：N 函数乘 M hook 批量挂接，支持 `:append/:local/:depth/:remove`。
- `dream-setq-hook` / `dream-unsetq-hook`：按 mode hook 设置 buffer-local 值并生成具名函数。
- `dream-defadvice` / `dream-undefadvice`：定义并挂接 advice。
- `dream-letf`：测试与临时场景下重绑函数、宏或 advice。
- `dream-with-suppressed-messages` / `dream-with-suppressed-output`：不重定义 Emacs primitive 的
  静默求值边界。

## 性能门

benchmark schema 3 使用 2 次 warmup 和 20 个独立 Emacs 进程。child 只接收一段自包含表单，
计时前不 preload `dream-benchmark`、`dream-paths` 或 `dream-startup`，因此 startup 覆盖真实
`early-init.el` + `init.el` 全路径。它记录 startup、first input、first file、first prog、环境快照
应用与每个 idle item，p95 使用 nearest-rank。基线同时绑定 schema、采样数、Emacs 版本、
`system-configuration` 和 native ABI。每项 p50/p95 相对基线不得回归超过 10%；
`idle-slice-max` p95 还有独立 50ms 上限，建立基线同样执行该硬门。

对超过预算的 idle 项，优先删除无条件预加载并保留真实 package 使用边界，不能通过提高预算掩盖。
当前 `lsp-mode`、`git-commit`、`with-editor` 和 `transient` 均随真实功能按需加载。macOS batch 与
daemon GUI 纳入本地验收；Windows GUI 在有对应环境前保持显式未验证状态。
