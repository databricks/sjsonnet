# Scala Native 架构级优化方案：提升 sjsonnet 性能

## 目录
1. [现状分析](#1-现状分析)
2. [优化方案清单](#2-优化方案清单)
3. [按优先级排序的实施路线图](#3-按优先级排序的实施路线图)

---

## 1. 现状分析

### 1.1 sjsonnet 在 Scala Native 上的瓶颈链

```
源码字节 → Parser → Expr(UTF-16) → StaticOptimizer → Evaluator → Val → Materializer → ujson.Value → Renderer → 输出
         │                                    │           │              │
         │ ~10层虚方法调用/builtin              │ GC压力     │ 对象分配
         │ 边界检查无法消除                     │ write barrier
         │ 无JIT内联                           │
```

### 1.2 从源码看到的关键事实

**字符串表示** (`javalib/src/main/scala/java/lang/String.scala:28-31`):
```scala
protected[_String] var value: Array[Char] = _   // UTF-16, 2 bytes/char
protected[_String] var offset: Int = 0
protected[_String] var count: Int = 0
protected[_String] var cachedHashCode: Int = _
```
- 无 Compact String（Latin-1 字符也用 2 字节存储）
- 无 String Deduplication
- `intern()` 是空操作（`String.scala:559`）
- hashCode 使用 `data(i) + ((hash << 5) - hash)` 逐 char 计算（`String.scala:396-404`）

**内联策略** (`interflow/Inline.scala:42-51`):
```scala
case build.Mode.ReleaseFull =>
  alwaysInline || hintInline || isSmall || isCtor || hasVirtualArgs
```
- `smallFunctionSize` 默认仅 12 条指令（`OptimizerConfig.scala:44`）
- `maxCalleeSize` 默认仅 256 条指令
- `hasVirtualArgs` 仅在 `ReleaseFull` 模式下触发内联

**虚方法分派** (`codegen/Lower.scala:1099-1216`):
- 虚方法 → vtable 间接调用（load rtti → load vtable pointer → load method pointer）
- trait 方法 → itable lookup（hash lookup + 验证 + 间接调用）
- 即使只有一个实现体，也需要完整的 vtable 查找

**GC 分配** (`GC.scala:15-25`):
```scala
private[runtime] def alloc(cls: Class[_], size: Int): RawPtr = extern
private[runtime] def alloc_small(cls: Class[_], size: Int): RawPtr = extern
private[runtime] def alloc_large(cls: Class[_], size: Int): RawPtr = extern
private[runtime] def alloc_array[T <: Array[_]](cls: Class[T], length: Int, stride: Int): RawPtr = extern
```
- 每次对象分配都是 extern call（需切换线程状态）
- Immix GC 的 bump allocator 在 `Allocator.h` 中实现，cursor/limit 快速路径在 C 层
- 无逃逸分析，无栈上分配（Val.Virtual 机制部分缓解）

**数组边界检查** (`nativelib/.../runtime/Arrays.scala:176-181`):
```scala
@inline def atRaw(i: Int): RawPtr =
  if (i < 0 || i >= length) {
    throwOutOfBounds(i, length)
  } else {
    atRawUnsafe(i)
  }
```
- Interflow 的 `genGuardInBounds` (`Lower.scala:642-662`) 生成 `Sge(idx, 0) && Slt(idx, len)` 双重检查
- 缺少范围分析（range analysis）来消除已知安全的检查

---

## 2. 优化方案清单

### 2.1 [HIGH] Compact String（UTF-8 / Latin-1 压缩）

**问题**: `java.lang.String` 使用 `Array[Char]`（UTF-16），Jsonnet 源码和 JSON 内容主要是 ASCII/Latin-1，2x 内存浪费。

**实现方案**:

在 `javalib/src/main/scala/java/lang/String.scala` 中添加 `coder` 字段：
```scala
protected[_String] var value: Array[Byte] = _  // 统一用 byte[]
protected[_String] var coder: Byte = 0          // 0=Latin1, 1=UTF16
```

需要修改的文件：
- `javalib/src/main/scala/java/lang/String.scala` — 核心 String 类
- `nir/src/main/scala/scala/scalanative/nir/Rt.scala:53-63` — String 字段定义
- `nativelib/.../gc/immix_commix/headers/ObjectHeader.h:87-99` — StringObject C 结构
- `tools/src/main/scala/scala/scalanative/codegen/Lower.scala:41-46` — stringFieldNames

sjsonnet 侧适配：
- 修改 `Val.Str` 的内部表示，直接使用 Scala Native 的压缩 String
- `Parser` 生成的字符串直接走压缩路径

**预期收益**: 内存带宽 ~2x，GC 压力降低 ~30-40%（ASCII 密集型负载），L1/L2 cache 命中率提升

**实现难度**: ⭐⭐⭐⭐ (高) — 需要改 string 底层表示 + 所有 string 操作适配

**需要修改 IR/代码生成**: 否（纯 javalib 层面修改，但 Rt.scala 需更新字段定义）

---

### 2.2 [HIGH] 增强内联策略 — 放宽小函数阈值

**问题**: `smallFunctionSize=12` 对于 sjsonnet 的 Val 访问器（`asInstanceOf`, `isInstanceOf`, getter）太保守。

**实现方案**:

在 sjsonnet 的 build 中配置：
```scala
nativeConfig ~= { _.withOptimizerConfig(
  _.withSmallFunctionSize(48)
    .withMaxCalleeSize(512)
    .withMaxInlineDepth(48)
) }
```

更进一步，修改 `interflow/Inline.scala:42-51`，在 `ReleaseFull` 模式下增加：
- **类型传播内联**: 如果调用者已知 receiver 的精确类型（`ExactClassRef`），即使 callee 超过 smallFunctionSize 也强制内联
- **热路径标记**: 为 sjsonnet 的 `Evaluator.eval`、`Val.force` 等热路径添加 `@AlwaysInline` 注解

**预期收益**: 减少 ~30-50% 的虚方法调用开销，直接消除间接跳转 + 分支预测失败

**实现难度**: ⭐⭐ (低) — 只需修改构建配置或小幅修改 Inline.scala

**需要修改 IR/代码生成**: 否

---

### 2.3 [HIGH] 单实现虚方法去虚化（Devirtualization）

**问题**: Interflow 的 `PolyInline` (`PolyInline.scala:30-35`) 仅在 `ReleaseFull` 模式下做多态内联，且限制 `classesCount <= 16 && implsCount >= 2 && implsCount <= 4`。对于只有单一实现的虚方法，完全没有去虚化。

**实现方案**:

在 `interflow/Inline.scala` 的 `shallInline` 方法中增加：
```scala
def isSingleImpl: Boolean = {
  val targets = analysis.infos.get(name.top).collect {
    case scope: ScopeInfo => scope.targets(name.sig)
  }
  targets.exists(_.size == 1)
}

// 新增规则
case build.Mode.ReleaseFull =>
  alwaysInline || hintInline || isSmall || isCtor || hasVirtualArgs || isSingleImpl
```

同时在 `codegen/Lower.scala:1159-1177` 的 `genMethodLookup` 中：
```scala
case Seq(impl) =>
  // 已有: 直接静态调用
  let(n, nir.Op.Copy(nir.Val.Global(impl, nir.Type.Ptr)), unwind)
```
这部分已经处理了单实现情况，但前提是 Interflow 已经将方法调用解析到这个分支。需要确保 Interflow 对单实现方法不做 BailOut。

**预期收益**: 消除 sjsonnet 中 ~60-70% 的虚方法调用（大量 trait 方法只有单实现）

**实现难度**: ⭐⭐⭐ (中) — 需要修改 Interflow 的内联判断逻辑

**需要修改 IR/代码生成**: 否

---

### 2.4 [HIGH] 减少对象分配 — Val 的值类型特化

**问题**: sjsonnet 的 `Val` 是 sealed trait，`Val.Num`, `Val.Str`, `Val.Bool` 等都是堆分配对象。每次数值计算、字符串拼接都产生 GC 压力。

**实现方案（sjsonnet 侧）**:

```scala
// 当前
sealed trait Val
case class Num(value: Double) extends Val
case class Str(value: String) extends Val
case class Bool(value: Boolean) extends Val

// 优化: 使用 tagged union 或 NaN boxing
object Val {
  // NaN Boxing: Double 的 NaN 空间编码其他类型
  // 0x7FF8000000000001 = true, 0x7FF8000000000000 = false
  // 0x7FF8000000000002 = null
  // Pointer values 用其他 NaN 空间编码
  type Val = Long  // 所有 Val 值用一个 Long 表示
}
```

或更保守的方案：将 `Val.Num` 改为 `AnyVal`：
```scala
final class ValNum(val value: Double) extends AnyVal with Val
```

**预期收益**: 减少 ~40-60% 的小对象分配，GC 暂停时间显著降低

**实现难度**: ⭐⭐⭐⭐ (高) — 需要重构 sjsonnet 的核心数据模型

**需要修改 IR/代码生成**: 否

---

### 2.5 [MEDIUM] 数组边界检查消除 — Range Analysis

**问题**: Interflow 的 `genGuardInBounds` (`Lower.scala:642-662`) 对每次数组访问都生成 `idx >= 0 && idx < length` 检查。循环中的已知安全访问无法消除。

**实现方案**:

在 `interflow/Eval.scala` 中添加简单的 range tracking：
```scala
// 为每个 Int 值维护 [min, max] 区间
case nir.Op.Bin(nir.Bin.Iadd, _, l, r) =>
  val lRange = getRange(l)
  val rRange = getRange(r)
  setRange(result, Range(lRange.min + rRange.min, lRange.max + rRange.max))

// 数组访问时检查 range 是否在 [0, length) 内
def canEliminateBoundsCheck(idx: Val, arr: Val): Boolean = {
  val range = getRange(idx)
  val arrLen = getArrayLength(arr)
  range.min >= 0 && range.max < arrLen
}
```

同时在 LLVM 层面，确保 `-O2` 或 `-O3` 传递给 clang，让 LLVM 的 SCEV (Scalar Evolution) 做循环边界消除。

**预期收益**: 热循环中减少 ~20-30% 的分支指令

**实现难度**: ⭐⭐⭐ (中)

**需要修改 IR/代码生成**: 否（Interflow 层面实现）

---

### 2.6 [MEDIUM] GC 分配快速路径内联化

**问题**: 每次 `GC.alloc` 都是 extern call。Immix 的 bump allocator 快速路径（`cursor < limit ? cursor : slow_path`）在 C 层，无法被 LLVM 内联。

**实现方案**:

在 `nativelib/src/main/scala/scala/scalanative/runtime/GC.scala` 中添加内联快速路径：
```scala
@alwaysinline
def allocFast(cls: Class[_], size: Int): RawPtr = {
  val thread = MutatorThread.current()
  val cursor = thread.allocator.cursor
  val limit = thread.allocator.limit
  val newCursor = elemRawPtr(cursor, size)
  if (castRawPtrToLong(newCursor) <= castRawPtrToLong(limit)) {
    thread.allocator.cursor = newCursor
    storeObject(cursor, cls) // 设置 rtti
    cursor
  } else {
    alloc(cls, size) // 慢路径
  }
}
```

需要将 Allocator 的 cursor/limit 暴露为 thread-local 或通过 `@struct` 访问。

**预期收益**: 小对象分配从 ~20ns 降到 ~5ns（消除 extern call + 线程状态切换）

**实现难度**: ⭐⭐⭐⭐ (高) — 需要修改 GC 运行时的 C 代码和 Scala 接口

**需要修改 IR/代码生成**: 是（需要确保 thread-local 访问被正确编译）

---

### 2.7 [MEDIUM] String.intern() 实现 — 字符串去重

**问题**: `String.intern()` 是空操作 (`String.scala:559`)。sjsonnet 中大量重复的字符串字面量（字段名、关键字）无法共享。

**实现方案**:

```scala
// 在 String.scala 中实现简单的 hash-consing
object _String {
  private val internTable = new java.util.concurrent.ConcurrentHashMap[String, String]()

  def intern(s: String): String = {
    internTable.computeIfAbsent(s, identity)
  }
}
```

同时在 sjsonnet 的 `Parser` 中，对标识符和关键字调用 `intern()`。

**预期收益**: 减少 ~15-25% 的字符串对象数量，GC 扫描时间降低

**实现难度**: ⭐⭐ (低)

**需要修改 IR/代码生成**: 否

---

### 2.8 [MEDIUM] 评估器管道扁平化 — 减少间接调用层数

**问题**: sjsonnet 的 `Evaluator.eval` 通过 `Expr` 的 pattern match 分发，每种表达式类型至少 1-2 层虚方法调用。加上 `Val.force`, `Materializer` 等，总共约 8-10 层间接调用。

**实现方案**:

在 `Expr.scala` 中使用 byte tag 替代 sealed trait pattern match：
```scala
// 在 StaticOptimizer 中为每个 Expr 分配 tag
object ExprTag {
  final val Literal = 0
  final val Identifier = 1
  final val BinaryOp = 2
  // ...
}

// Evaluator.eval 使用 switch 替代 match
def eval(expr: Expr): Val = (expr.tag: @switch) match {
  case ExprTag.Literal => expr.asInstanceOf[Expr.Literal].value
  case ExprTag.Identifier => resolveVar(expr.asInstanceOf[Expr.Identifier])
  // ...
}
```

如果 `ExprTags` 已经存在（在 `Expr.scala` 中），确保 `Evaluator.eval` 使用 `@switch` 而非 pattern match。

**预期收益**: 减少 ~15% 的分支预测失败，eval 循环更紧凑

**实现难度**: ⭐⭐ (低) — sjsonnet 已有 ExprTags 基础设施

**需要修改 IR/代码生成**: 否

---

### 2.9 [LOW] LLVM Pass Pipeline 调优

**问题**: Scala Native 生成的 LLVM IR 可能没有充分利用 LLVM 的优化 pass。

**实现方案**:

在 `tools/src/main/scala/scala/scalanative/build/Build.scala` 中调整 LLVM 编译参数：
```
-O3                          # 而非 -O2
-march=native                # 针对本机 CPU 微架构
-flto=thin                   # 链接时优化
-fvectorize-loops            # 循环向量化
-fslp-vectorize              # SLP 向量化
-ffast-math                  # 浮点数学快速路径（sjsonnet 的 Num 计算）
-funroll-loops               # 循环展开
-finline-hint-threshold=100  # 提高内联阈值
```

**预期收益**: ~10-20% 整体性能提升（取决于 LLVM 版本和目标架构）

**实现难度**: ⭐ (极低) — 只需修改构建参数

**需要修改 IR/代码生成**: 否

---

### 2.10 [LOW] 逃逸分析 + 栈上分配

**问题**: Interflow 有 `Val.Virtual` 机制（`Instance.scala`），但仅限于 Interflow 能追踪的简单模式。sjsonnet 中间结果（如 `Val.Lazy` 的闭包）大多逃逸。

**实现方案**:

扩展 Interflow 的 `State` 以追踪更多分配模式：
```scala
// 在 State.scala 中，对以下模式标记为栈分配候选:
// 1. 在单个 eval 调用内创建并消费的 Val
// 2. 不被闭包捕获的局部对象
// 3. 不存储到全局/堆上的临时对象
```

这需要在 `interflow/State.scala` 中实现 escape analysis pass。

**预期收益**: 减少 ~20-30% 的堆分配

**实现难度**: ⭐⭐⭐⭐⭐ (极高) — 需要精确的逃逸分析，错误会导致 use-after-free

**需要修改 IR/代码生成**: 可能需要在 NIR 中添加 `Op.Stackalloc` 的对象版本

---

### 2.11 [LOW] Write Barrier 优化

**问题**: Immix/Commix GC 的每次引用字段写入都需要 write barrier。sjsonnet 的 `Val.Obj` 频繁更新字段。

**实现方案**:

1. **Card Marking 优化**: 将逐字段的 barrier 改为 card table marking
2. **Young Generation 假设**: 对新分配的对象（< N 字节 age），跳过 barrier（因为 young objects 在 minor GC 中全部扫描）
3. **Interflow 层面**: 如果能证明写入的对象是当前分配周期内创建的（young），消除 barrier

在 `codegen/Lower.scala` 的 `genStoreOp` 中添加条件：
```scala
def needsWriteBarrier(obj: nir.Val, value: nir.Val): Boolean = {
  // 如果 obj 和 value 都是 young generation，跳过 barrier
  // 如果 obj 是 stack-allocated，跳过 barrier
  true // 保守默认
}
```

**预期收益**: 减少 ~10-15% 的 store 指令开销

**实现难度**: ⭐⭐⭐ (中)

**需要修改 IR/代码生成**: 是（需要修改 Lower.scala 的 store 生成逻辑）

---

## 3. 按优先级排序的实施路线图

### Phase 1: 快速收益（1-2 周）

| # | 优化 | 预期收益 | 实施位置 |
|---|------|---------|---------|
| 2.9 | LLVM 编译参数调优 | +10-20% | build 配置 |
| 2.2 | 放宽内联阈值 | +15-25% | build 配置 / Inline.scala |
| 2.8 | eval 使用 @switch | +5-10% | sjsonnet Evaluator.scala |

### Phase 2: 核心优化（2-4 周）

| # | 优化 | 预期收益 | 实施位置 |
|---|------|---------|---------|
| 2.3 | 单实现去虚化 | +20-30% | Inline.scala |
| 2.7 | String.intern() | +5-15% | String.scala + Parser.scala |
| 2.5 | 边界检查消除 | +10-15% | Eval.scala (Interflow) |

### Phase 3: 架构优化（4-8 周）

| # | 优化 | 预期收益 | 实施位置 |
|---|------|---------|---------|
| 2.1 | Compact String | +30-50% 内存 | String.scala + Rt.scala + ObjectHeader.h |
| 2.4 | Val 值类型特化 | +20-40% GC | sjsonnet Val.scala |
| 2.6 | GC 快速路径内联 | +15-25% 分配 | GC.scala + Allocator C code |

### Phase 4: 高级优化（8+ 周）

| # | 优化 | 预期收益 | 实施位置 |
|---|------|---------|---------|
| 2.10 | 逃逸分析 | +15-25% 分配 | Interflow State.scala |
| 2.11 | Write Barrier 优化 | +5-10% store | Lower.scala |

---

## 附录：关键源码位置索引

| 组件 | 文件 | 关键行 |
|------|------|--------|
| String 底层表示 | `javalib/.../String.scala` | L28-31 |
| String hashCode | `javalib/.../String.scala` | L390-409 |
| 内联策略 | `tools/.../interflow/Inline.scala` | L14-91 |
| 内联配置 | `tools/.../build/OptimizerConfig.scala` | L38-75 |
| 多态内联 | `tools/.../interflow/PolyInline.scala` | L8-53 |
| 常量折叠 | `tools/.../interflow/Combine.scala` | 全文 |
| 虚方法分派 | `tools/.../codegen/Lower.scala` | L1099-1216 |
| 边界检查生成 | `tools/.../codegen/Lower.scala` | L642-662 |
| 对象分配 | `tools/.../codegen/Lower.scala` | L1424-1465 |
| GC 接口 | `nativelib/.../runtime/GC.scala` | L14-25 |
| GC Allocator | `nativelib/.../gc/immix/Allocator.h` | L11-37 |
| 对象头结构 | `nativelib/.../gc/immix_commix/headers/ObjectHeader.h` | L65-99 |
| 数组实现 | `nativelib/.../runtime/Arrays.scala` | 全文 |
| Intrinsics | `nativelib/.../runtime/Intrinsics.scala` | 全文 |
| NIR Runtime 类型 | `nir/.../nir/Rt.scala` | L6-101 |
| 运行时类型信息 | `tools/.../codegen/RuntimeTypeInformation.scala` | - |
| VTable | `tools/.../codegen/VirtualTable.scala` | 全文 |
