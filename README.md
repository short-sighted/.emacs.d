# Dream Emacs Configuration

面向 Emacs 31、由 Borg/epkg 管理包、setup.el 负责声明式配置的个人配置。
启动路径保持显式且可审计，昂贵功能通过 `once` 增量协调，逐 buffer 行为仍由真实 mode hook 承担。

## 布局

```text
early-init.el             启动前策略
init.el                   显式顶层装配
core/dream-hooks.el       一次性启动生命周期 hook 与错误隔离
core/dream-defaults.el    Emacs 31 基线默认值与统一 escape
core/dream-*.el           其余启动、路径、setup 与 autoload 基础设施
lib/dream-*.el            可独立加载的扩展库
lisp/init-*.el            编辑器与工具领域配置
lisp/lang/init-lang-*.el  编程通用层与语言集成
build/                    autoload、编译和 manifest 工具
test/                     ERT、smoke 与多进程 benchmark
site-lisp/                Borg drones
```

完整的职责、命名、加载语义和反例见 [docs/conventions.md](docs/conventions.md)。

## 构建

```text
make quick   -> Borg 字节码包 -> 聚合 autoload -> 自有配置字节码
make native  -> Borg 原生包   -> 聚合 autoload -> 自有配置默认策略
                                      默认策略: byte

make config-build   仅重建自有字节码，并删除自有 ELN
make config-native  可选地原生编译自有配置
make check          ERT
make check-isolated 每个自有文件在全新 Emacs 中独立严格编译
make check-declare  校验残余 declare-function 与真实定义
make smoke          冷启动 smoke
make benchmark      对长期基线执行回归门禁
```

构建器扫描 `lib/dream-*.el` 与 `lisp/**/init-*.el`，尊重 `.nosearch`，拒绝全局重复 basename。
manifest 分别记录包与自有配置的 native 状态；切回 byte 只删除当前自有配置对应的 ELN。

### ELC 编译前提

**开启自有配置 ELC/ELN 编译，就必须同时处理每个文件的编译器依赖。** 运行期惰性加载
并不等于编译器认识包里的变量和非 autoload 函数。运行期真依赖使用顶层 `require`；仅供
编译器读取符号定义的依赖，必须在文件头写入：

```elisp
(cl-eval-when (compile)
  (require 'package-feature))
```

不得用无来源的 `defvar` 或 `declare-function` 存根压制警告。共享构建会话可能因前一个文件
已经加载 package 而侥幸通过，所以启用 ELC 后，`make config-build`、`make check-isolated` 和
`make check-declare` 共同构成编译验收；缺少其中的依赖治理，就不应开启自有配置严格编译。
完整决策链见 [编译期声明规范](docs/conventions.md#编译期声明规范)。

## 性能结论

2026-07-11 在 Emacs `31.0.90-760d5705`、Apple Silicon
(`aarch64-apple-darwin25.5.0`) 上，以 2 次 warmup 和 15 个独立进程采样：

| 模式 | startup p50/p95 | first input p50/p95 | first prog p50/p95 | idle max p50/p95 |
| --- | --- | --- | --- | --- |
| 原始基线 | 27.2/27.8 ms | 14.6/14.8 ms | 152.9/154.4 ms | 96.1/96.7 ms |
| byte A | 16.2/16.6 ms | 7.8/8.0 ms | 143.0/143.5 ms | 87.1/88.7 ms |
| native | 42.7/43.4 ms | 8.1/8.3 ms | 151.5/154.2 ms | 92.7/94.0 ms |
| byte B | 16.5/16.9 ms | 7.7/8.0 ms | 142.7/145.1 ms | 87.5/88.0 ms |

byte A/B 的 startup p50 差约 2%，低于 5% 噪声门槛；受控实验中的 native 只改变
自有配置产物且明显不占优，所以 `dream-build-default-config-native` 保持 `nil`。随后完成完整
`make native` 组合验证，最终日常状态为“第三方包 native + 自有配置 byte”；该状态的新长期基线
startup p50/p95 为 25.9/26.7 ms，紧随其后的门禁复测为 26.5/26.8 ms。
这些数字只代表上述 macOS/ABI 环境；Windows 尚未实测。

编译期声明迁移使 `make config-build` 中位时长从 `0.60s` 增至 `1.05s`（`+0.45s`），
`make check-isolated` 为 `5.83s`。声明不会进入 `.elc`；迁移后的 benchmark 为 startup
`25.3/25.6ms`、first input `14.4/14.7ms`、first prog `155.6/156.8ms`、idle max
`95.4/96.3ms`（p50/p95），相对长期基线无启动回归。
