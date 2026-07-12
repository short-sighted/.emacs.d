# Dream Emacs Configuration

面向 Emacs 31、由 Borg/epkg 管理包、setup.el 负责声明式配置的个人配置。启动路径显式且
可审计；`once` 只协调一次性依赖和空闲增量加载，逐 buffer 行为仍由真实 mode hook 承担。

## 布局

```text
early-init.el             Emacs 31 启动与性能策略
init.el                   显式顶层装配
core/dream-core.el        错误、日志、hook/advice 基础 API
core/dream-hooks.el       一次性编辑器生命周期
core/dream-runtime.el     环境快照与零运行期编译策略
core/dream-fonts.el       按 display 探测、逐 frame 应用的字体机制
core/dream-defaults.el    Emacs 31 基线默认值与统一 escape
core/dream-*.el           其余路径、startup、setup 与 autoload 基础设施
lib/dream-*.el            可独立加载的扩展库
lisp/init-*.el            编辑器与工具领域配置
lisp/lang/init-lang-*.el  编程通用层与语言集成
build/                    autoload、显式编译和 manifest 工具
test/                     ERT、smoke 与多进程 benchmark
site-lisp/                Borg drones
```

完整职责、命名、加载语义和反例见 [docs/conventions.md](docs/conventions.md)。

## 构建

```text
make quick   -> Borg 字节码包 -> 聚合 autoload -> runtime artifacts -> 自有配置 ELC
make native  -> Borg 原生包   -> 聚合 autoload -> runtime artifacts -> 自有配置默认策略
                                                        默认策略: byte

make runtime-artifacts  刷新 shell 环境快照与 ABI trampoline
make config-build       重建自有 ELC，并删除仅属于自有配置的 ELN
make config-native      可选地同步原生编译自有配置
make check              ERT
make check-isolated     每个自有文件在全新 Emacs 中独立严格编译
make check-declare      校验残余 declare-function 与真实定义
make runtime-no-compile 在正常工作流前后比较全部 ELC/ELN 签名
make smoke              冷启动 smoke
make benchmark-baseline 建立当前 Emacs/ABI 的长期基线
make benchmark          执行相对回归门与 50ms idle p95 硬门
```

构建器扫描 `lib/**/dream-*.el` 与 `lisp/**/init-*.el`，尊重 `.nosearch`，拒绝全局重复
basename，并清除失去对应源码的自有 ELC。manifest 把 `early-init.el` 也视为输入，记录源码
hash、Emacs/系统/native ABI、环境快照、trampoline，以及包和自有配置各自的 native 状态。
单独构建一个 Borg drone 只刷新聚合 autoload，不会冒充一次完整配置构建而重写 manifest。

`early-init.el` 为了稳定启动成本而设置 `load-prefer-newer=nil`。因此源码改动后必须显式运行
`make config-build`；旧 ELC 会继续生效到下一次构建，运行期不会编译或自动替换它。

### ELC 编译前提

**开启自有配置 ELC/ELN 编译，就必须同时处理每个文件的编译器依赖。** 运行期惰性加载
并不等于编译器认识包里的变量和非 autoload 函数。运行期真依赖使用顶层 `require`；仅供
编译器读取符号定义的依赖，必须在文件头写入：

```elisp
(cl-eval-when (compile)
  (require 'package-feature))
```

不得用无来源的 `defvar` 或 `declare-function` 存根压制警告。共享构建会话可能因前一个文件
已经加载 package 而侥幸通过，所以 `make config-build`、`make check-isolated` 和
`make check-declare` 共同构成 ELC 验收。完整规则见
[编译期声明规范](docs/conventions.md#编译期声明规范)。

### 零运行期编译

正常启动会关闭 native JIT，并守卫 `byte-compile-file`、`native-compile`、
`native-compile-async` 与 `comp-trampoline-compile`。配置和包产物只能由显式构建命令生成；
缺失的 primitive trampoline 会 fail closed，不会在首次使用时现场编译。

`make runtime-artifacts` 是有意允许编译和执行 login shell 的边界：它为当前 ABI 预建所需
trampoline，并在 macOS 上把选定环境变量写成权限 `0600` 的 inert Lisp data。GUI/daemon
启动只读取兼容快照，不同步执行 shell。Elisp Flymake 的临时诊断和外部语言编译器不属于持久
配置产物，仍可正常工作。没有 native compiler 的 Emacs 31 使用空 trampoline 契约，byte-only
配置仍受同一运行期编译守卫保护。

## 字体

字体由 `core/dream-fonts.el` 统一管理。可设置 `dream-font`、
`dream-variable-pitch-font`、`dream-serif-font`、`dream-symbol-font`、`dream-emoji-font`、
`dream-cjk-font`、`dream-font-size` 和 `dream-font-size-step`。值为 nil 时使用平台 fallback；
显式字体缺失会报告 `dream-font-error`，不会静默换成另一款字体。

交互命令为 `dream-font-reload`、`dream-font-increase`、`dream-font-decrease` 和
`dream-font-reset`。字体 family 探测结果按 display 缓存，face 和 fontset 按 frame 只设置一次；
`after-make-frame-functions` 保证非 server 创建的后续 frame 也会应用。手动 reload 会恢复 Dream
管理的 CJK rescale、清 Emacs 字体缓存并重放所有 GUI frame。终端会话为 no-op。

## 性能结论

2026-07-12 在 Emacs `31.0.90-760d5705`、Apple Silicon
(`aarch64-apple-darwin25.5.0`) 上，以 2 次 warmup 和 20 个独立进程采样：

| 样本 | startup p50/p95 | first input | first file | first prog | environment | idle max |
| --- | --- | --- | --- | --- | --- | --- |
| schema 3 基线 | 32.7/34.9 ms | 24.1/24.9 ms | 13.4/13.9 ms | 142.1/143.6 ms | 0.128/0.163 ms | 33.1/34.3 ms |
| 紧随复验 | 32.4/33.5 ms | 24.1/25.0 ms | 13.4/14.0 ms | 141.7/144.3 ms | 0.128/0.142 ms | 33.5/34.5 ms |

`lsp-mode`、`git-commit`、`with-editor` 和 `transient` 不再无条件 idle 预加载；最慢剩余项为
`markdown-mode`，复验 p95 为 `34.511ms`，低于独立的 50ms idle slice 硬门。schema 3 的 child
在计时前不再预载任何 Dream 库，因此 startup 包含真实的 `early-init.el` + `init.el` 全路径，不能
与旧 schema 2 数字直接比较。先前 A/B/A 实验已证明自有配置 native 在该机器上不占优，因此
`dream-build-default-config-native` 保持 nil，日常状态仍是“第三方包 native，自有配置 byte”。
这些数字只代表上述 macOS/ABI；Windows GUI 尚未实测。
