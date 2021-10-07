# Inkstone 语言的设计驱动

Inkstone 语言的设计目标是作为应用程序（主要是游戏）中的 “胶水” 脚本语言使用。它可以通过内部的控制流驱动宿主程序的运行，比如控制游戏路径、对话内容等等。在设计上，Inkstone 语言不会承担任何繁重的计算任务。

当前常见的与 Inkstone 在同一用途的语言包括：

- [RenPy](https://www.renpy.org/)
- [Ink](https://github.com/inkie/ink)
- [Kirikiri Adventure Game (KAG)](http://kirikirikag.sourceforge.net/contents/index.html)

这些语言要么功能太弱（多数只是领域特定的标记语言），要么语法太难看（尤其是符号太多），要么两者兼有。因此，Inkstone 试图通过合适的语法、特性和标准库实现一个全功能的胶水语言。

## 特性与取舍

Inkstone 当前的目标 (Goals) 如下：

- 对宿主程序的控制流进行操纵
- 声明式的数据存储和控制流操纵
- 支持并优化尾递归调用（以支持网状控制流）
- 支持并优化异步/纤程操作（以支持用户互动）
- 在基础状态下长得非常像标记语言
- 一些可选功能
  - 运行状态快照

非目标 (Non-goals) 如下：

- 高性能或者复杂运算（请在宿主程序里做）

为了支持上述功能，Inkstone 目前暂定将会这样设计：

- 非纯函数式语言，混合范式，及早求值
- 动态强类型，不设结构体，但允许用户使用 Tuple, Array 和 HashMap 生成自定义类型（参考 Elixir）
  - 可能会使用不可变数据类型存储状态
  - 可能会另外设计一个可选的类型标注方法和类型检查
- 动态派发/面向对象特性（可能会使用原型链或多派发实现）
- 化简这些语法：函数调用、函数定义、对象构造、lambda
  - 当前设计稿比较贴近 Ruby 的风格，混合了一些 Rust 和 Haskell
- 内置支持 yield 和 coroutine（可能是以 channel/mailbox 的形式）
- 带命名空间的外部函数导入机制

设计过程中参考的（和将会参考的）语言：

- 语法设计
  - Elixir
  - Haskell
  - RenPy
  - Ruby
  - Rust
- 动态派发
  - Elixir
  - Javascript
  - Julia
  - Lua
- 类型系统
  - Elixir
  - Julia
  - TypeScript
- 纤程
  - Elixir
  - Lua

