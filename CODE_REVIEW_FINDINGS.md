# sjsonnet 代码审查发现

> 审查时间: 2026-06-10（深度审查）
> 审查范围: Evaluator, Val, Materializer, StaticOptimizer, 标准库, Parser, 测试套件
> 参考实现: go-jsonnet, jrsonnet, C++ jsonnet
> 审查模式: 🐲深度 · 🧠最强大脑（统帅+分析师） · 九令洞鉴全量

---

## 🔴 Blocker (必须修复)

### B1. 函数相等性检查不一致
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:1448-1449, 1455-1456`
- **问题**: `==`/`!=` 仅在两侧都是函数时报错（使用 `&&`），参考实现在任一侧为函数时即报错（应使用 `||`）
- **当前代码**:
  ```scala
  if (l.isInstanceOf[Val.Func] && r.isInstanceOf[Val.Func])
    Error.fail("cannot test equality of functions", pos)
  ```
- **参考实现验证**:
  - **go-jsonnet** `builtins.go:rawEquals`:
    ```go
    case *valueFunction:
        return false, i.Error("Cannot test equality of functions")
    ```
  - **C++ Jsonnet** `desugarer.cpp`: `BOP_MANIFEST_EQUAL` desugar 为 `std.equals` 调用，`std.equals` 内部调用 `primitiveEquals`，对函数报错
- **影响**: `function(x) x == 42` 在 sjsonnet 返回 `false`，在 go-jsonnet/C++ Jsonnet 报错
- **修复方案**: 修改检查条件为 `||`
  ```scala
  if (l.isInstanceOf[Val.Func] || r.isInstanceOf[Val.Func])
    Error.fail("cannot test equality of functions", pos)
  ```
- **测试影响**: 需要更新 `EvaluatorTests.scala` 中的 `functions` 和 `functionEqualsNull` 测试用例

### B2. 断言消息类型处理
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:1153, 1723`
- **状态**: ✅ 已正确实现，统一使用 `materializeError` 处理断言消息
- **验证**: 顶层断言和对象内断言都使用相同的方法

### B3. 静态优化器修改共享单例的 pos
- **文件**: `sjsonnet/src/sjsonnet/StaticOptimizer.scala:149,152`
- **问题**: `rhs.pos = pos` 可能修改 `staticTrue`/`staticFalse` 单例的 `pos`
- **当前代码**:
  ```scala
  case And(pos, _: Val.True, rhs: Val.Bool) => rhs.pos = pos; rhs  // 危险
  case Or(pos, _: Val.False, rhs: Val.Bool) => rhs.pos = pos; rhs  // 危险
  ```
- **风险**: 如果 `rhs` 是 `Val.bool(b)` 返回的单例（`staticTrue`/`staticFalse`），修改 `pos` 会影响所有后续使用，导致错误消息位置信息错误
- **修复方案**: 创建新实例而非修改原实例
  ```scala
  case And(pos, _: Val.True, rhs: Val.Bool) =>
    rhs match {
      case _: Val.True  => Val.True(pos)
      case _: Val.False => Val.False(pos)
      case _            => rhs.pos = pos; rhs
    }
  case Or(pos, _: Val.False, rhs: Val.Bool) =>
    rhs match {
      case _: Val.True  => Val.True(pos)
      case _: Val.False => Val.False(pos)
      case _            => rhs.pos = pos; rhs
    }
  ```

---

## 🟡 Suggestion (建议修复)

### S1. Auto-TCO 遗漏 And/Or 守卫递归
- **文件**: `sjsonnet/src/sjsonnet/StaticOptimizer.scala:702-712`
- **问题**: `hasNonRecursiveExit` 忽略 `And`/`Or` 的 lhs 提供的基例
- **影响**: `local f(x) = x > 0 && f(x - 1)` 无法获得自动尾调用优化
- **修复方案**: 检查 lhs 是否为非递归出口

### S2. super[key] 无超类时静默回退到 self
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:1237`
- **问题**: `visitLookupSuper` 在无超类时静默回退到 `self`，而 `visitSelectSuper` 报错
- **当前代码对比**:
  ```scala
  // visitSelectSuper (line 768) - 正确
  if (sup == null) Error.fail("Attempt to use `super` when there is no super class", e.pos)
  
  // visitLookupSuper (line 1237) - 不一致
  if (sup == null) sup = self  // 静默回退
  ```
- **参考实现验证**:
  - **jrsonnet**: 在无超类时报错 `NoSuperFound`
  - **go-jsonnet**: `objectIndex` 函数在无超类时报错
- **修复方案**: 统一行为，无超类时报错
  ```scala
  if (sup == null) Error.fail("Attempt to use `super` when there is no super class", e.pos)
  ```

### S3. std.atan2 参数名顺序
- **文件**: `sjsonnet/src/sjsonnet/stdlib/MathModule.scala:451`
- **状态**: ✅ 已正确实现，参数名 `"y", "x"` 与规范 `std.atan2(y, x)` 一致
- **代码**: `builtin("atan2", "y", "x") { (pos, ev, y: Double, x: Double) => math.atan2(y, x) }`

### S4. std.repeat 错误消息
- **文件**: `sjsonnet/src/sjsonnet/stdlib/ArrayModule.scala:991`
- **状态**: ✅ 已正确实现
- **代码**: `Error.fail("repeat requires count >= 0, got " + count)`

### S5. formatFloat 可能产生双倍 E+ 前缀
- **文件**: `sjsonnet/src/sjsonnet/Format.scala:979`
- **问题**: `.replace("E", "E+")` 如果 DecimalFormat 已经产生 `E+` 会变成 `E++`
- **影响**: 科学计数法格式错误
- **修复方案**: 使用更精确的替换逻辑，检查是否已有 `+` 号

### S6. LazyFunc.value 不处理 null 返回值
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:45-53`
- **问题**: 如果 thunk `f()` 返回 `null`，第二次访问会抛 NullPointerException
- **影响**: 调试困难，错误发生在第二次访问而非第一次
- **修复方案**: 使用哨兵值或 Option 包装

### S7. TailCall.resolve 布尔/错误检查交互
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:3093-3126`
- **问题**: 当内部和外部都有布尔检查时，外部检查位置被静默替换
- **影响**: 堆栈跟踪可能显示错误的位置信息
- **修复方案**: 保留外部布尔检查位置或合并信息

### S8. Branch elimination 修改可能共享节点的 pos
- **文件**: `sjsonnet/src/sjsonnet/StaticOptimizer.scala:136,139`
- **问题**: `thenExpr.pos = pos` 和 `elseExpr.pos = pos` 修改存活分支的位置
- **影响**: 如果表达式对象在多个 AST 位置共享（常量内联），位置修改会影响所有引用
- **修复方案**: 创建新实例而非修改原实例

---

## ⚪ Nit (可选改进)

### N1. % 0 产生 NaN 而非报错
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:248, 1345`
- **问题**: `tryInlineArith` 和 `visitBinaryOp` 中的 `%` 运算不检查除零
- **当前代码**:
  ```scala
  // tryInlineArith (line 248)
  case Expr.BinaryOp.OP_% =>
    Val.cachedNum(pos, ld % rd)  // 不检查除零
  
  // visitBinaryOp (line 1345)
  case Val.Num(_, rd) => Val.cachedNum(pos, ld % rd)  // 不检查除零
  ```
- **参考实现验证**:
  - **go-jsonnet** `builtins.go:builtinModulo`:
    ```go
    if y.value == 0 {
        return nil, i.Error("Division by zero.")
    }
    ```
  - **C++ Jsonnet**: `%` desugar 为 `std.mod` 调用，同样检查除零
- **影响**: `0.0 % 0.0` 和 `1.0 % 0.0` 产生 NaN，而参考实现报 "Division by zero."
- **修复方案**:
  - `tryInlineArith`: 添加 `if (rd == 0) null` 回退到慢路径
  - `visitBinaryOp`: 添加 `if (rd == 0) Error.fail("Division by zero.", pos)`

### N2. isNonCapturingBody/isInvariantExpr 保守排除
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:466-504,514-557`
- **问题**: 错过优化机会（Arr, Comp, IfElse, Lookup）
- **修复方案**: 扩展分析覆盖更多安全情况

### N3. checkStackDepth 部分重载未跟踪 maxStackDepth
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:49-59`
- **问题**: debug 统计可能低估最大栈深度
- **修复方案**: 统一所有重载的统计跟踪

### N4. cachedNum 丢弃调用者的位置信息
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:276-303`
- **问题**: 返回的 Num 使用合成位置 `(null, -1)` 而非调用者提供的位置
- **影响**: 错误消息中的位置信息可能不准确
- **修复方案**: 在需要精确位置的场景使用 `Val.Num(pos, ...)` 而非 `cachedNum`

### N5. Num 允许 NaN 但拒绝 Infinity
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:470-473`
- **问题**: 构造函数只检查 `isInfinite`，不检查 `isNaN`
- **当前代码**:
  ```scala
  final case class Num(var pos: Position, private val num: Double) extends Literal {
    if (num.isInfinite) {
      Error.fail("overflow")
    }
    // 缺少 isNaN 检查
  }
  ```
- **影响**: NaN 可以通过 `rawDouble` 传播，影响比较操作
- **注释**: 代码注释声称 "NaN values cannot arise from valid Jsonnet expressions"，但 `% 0.0` 可以产生 NaN
- **修复方案**: 考虑在构造函数中也拒绝 NaN，或在 `asDouble` 方法中添加检查

### N6. std.mantissa 和 std.exponent 对次正规数精度丢失
- **文件**: `sjsonnet/src/sjsonnet/stdlib/MathModule.scala:532-549`
- **问题**: 次正规 double（如 `5e-324`）的计算可能产生错误结果
- **修复方案**: 使用 `Math.getExponent` 和 `Math.scalb` 替代手动计算

### N7. LazyIndexedArr.eval 不缓存惰性包装器
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:862-871`
- **问题**: 每次调用 `eval(i)` 创建新包装器，浪费分配
- **修复方案**: 参考 `LazyViewArr.eval` 的缓存策略

### N8. Arr.reversed 共享相同的 backing arr 引用
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:799-804`
- **问题**: 返回的 Arr 与原始数组共享相同的 arr 引用
- **影响**: 如果原始数组的 arr 后续被修改，反转视图也会受影响
- **修复方案**: 考虑创建防御性副本

### N9. asLazyArray 对反转数组每次调用都重新分配
- **文件**: `sjsonnet/src/sjsonnet/Val.scala:670-684`
- **问题**: 不更新 `this.arr` 或重置 `_reversed` 标志
- **修复方案**: 缓存结果数组

### N10. visitInSuper 无超类时返回 staticFalse 而非报错
- **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:1312`
- **问题**: `visitInSuper` 在无超类时返回 `Val.staticFalse`，而参考实现可能报错
- **影响**: 语义差异
- **修复方案**: 考虑在 strict 模式下报错

---

## 📊 测试覆盖差距

### P0 (高优先级)

1. **YAML 块标量转义 TODO**
   - **文件**: `sjsonnet/src/sjsonnet/YamlRenderer.scala:55`
   - **问题**: `appendString(split)` 未转义特殊字符
   - **影响**: 可能产生无效 YAML
   - **修复方案**: 实现 YAML 块标量转义逻辑

2. **DecimalFormat 多位数整数部分模式测试缺失**
   - **文件**: `test/src/sjsonnet/DecimalFormatTests.scala:56-67`
   - **问题**: 6 个测试用例被注释掉
   - **影响**: 模式如 `0000.#` 未被测试
   - **修复方案**: 更新测试基础设施支持多位数模式

### P1 (中优先级)

3. **PrettyYamlRenderer 注释测试因 Mill 沙箱问题被跳过**
   - **文件**: `test/src-jvm-native/sjsonnet/PrettyYamlRendererTests.scala:34-38`
   - **问题**: YAML 注释保留功能未测试
   - **修复方案**: 解决 Mill 沙箱访问问题

4. **preserveOrder 模式下 manifestYamlDoc/manifestIni/manifestToml/manifestXmlJsonml 测试缺失**
   - **影响**: 顺序保留正确性未验证
   - **修复方案**: 添加相应测试用例

5. **std.sort 对对象/函数元素的错误路径未测试**
   - **影响**: 错误处理行为未验证
   - **修复方案**: 添加边界测试用例

### P2 (低优先级)

6. **数学函数边界情况测试不足**
   - 函数: `log2`, `log10`, `atan2`, `hypot`, `mantissa`, `exponent`
   - **影响**: NaN, Infinity, -0.0 等边界情况未覆盖
   - **修复方案**: 添加边界值测试

7. **ByteRenderer 与 Renderer 等价性测试缺失**
   - **影响**: 两种渲染器可能产生不同输出
   - **修复方案**: 添加系统性等价测试

8. **ParseCache/Lazy 线程安全测试缺失**
   - **影响**: 客户端-服务器模式可靠性未验证
   - **修复方案**: 添加并发测试

---

## 🔧 性能改进项

### 性能优化机会

1. **常量折叠错过 Num+Str/Str+Num/Obj+Obj**
   - **文件**: `sjsonnet/src/sjsonnet/StaticOptimizer.scala:459-465`
   - **影响**: 运行时处理这些情况
   - **修复方案**: 扩展 `tryFoldBinaryOp` 覆盖更多类型组合

2. **isInvariantExpr 错过 IfElse/Lookup/Comp**
   - **文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:514-557`
   - **影响**: 两级循环优化不触发
   - **修复方案**: 扩展分析覆盖更多表达式类型

3. **std.decodeUTF8 迭代数组两次**
   - **文件**: `sjsonnet/src/sjsonnet/stdlib/StringModule.scala:1182-1196`
   - **问题**: 验证和解码分开进行
   - **修复方案**: 合并为单次迭代

4. **std.lines 材料化数组两次**
   - **文件**: `sjsonnet/src/sjsonnet/stdlib/ManifestModule.scala:300-321`
   - **问题**: 验证和字符串构建分开进行
   - **修复方案**: 合并为单次处理

5. **Stackless 路径不使用内联对象快速路径**
   - **文件**: `sjsonnet/src/sjsonnet/Materializer.scala:489-506`
   - **影响**: 深层嵌套对象性能下降
   - **修复方案**: 在栈帧管理中支持内联迭代

---

## 📋 已知兼容性差异

1. **Unicode 代理处理**
   - sjsonnet 保留原始代理码点，go-jsonnet 替换为 U+FFFD
   - **文件**: `test/src/sjsonnet/UnicodeHandlingTests.scala:177`

2. **Base64 填充验证**
   - sjsonnet 严格填充验证，与 go-jsonnet 和 C++ jsonnet 对齐
   - **文件**: `test/src/sjsonnet/Base64Tests.scala:498`

3. **std.mergePatch 行为**
   - 匹配 google/jsonnet 和 go-jsonnet v0.20.0
   - **文件**: `test/src/sjsonnet/StdMergePatchTests.scala:7`

4. **数字解析器差异**
   - sjsonnet 解析 `1_200_.0` 为 `(1200)._0`（字段访问），go-jsonnet 报错
   - **文件**: `test/src/sjsonnet/ParserTests.scala:195-200`

5. **YAML 解析器差异**
   - sjsonnet 使用 SnakeYAML/scala-yaml，解析 `a: b:` 为 `{"a":"b:"}`
   - 参考实现使用 RapidYAML，会报错
   - **文件**: `test_suite/.sync_ignore`

---

## 🎯 修复优先级建议

### 立即修复 (本周)
1. **B1**: 函数相等性检查（改 `&&` 为 `||`）
2. **B3**: 静态优化器单例 pos 修改（创建新实例）
3. **N1**: % 0 产生 NaN（添加零除检查）
4. **S2**: super[key] 行为一致性（无超类时报错）

### 短期修复 (2周内)
1. **S5**: formatFloat E+ 处理
2. **S8**: Branch elimination pos 修改
3. **P0 测试覆盖差距**

### 中期修复 (1个月内)
1. **S1**: Auto-TCO And/Or 守卫
2. **S6-S7**: LazyFunc/TailCall 问题
3. **N5**: Num NaN 处理策略
4. **P1 测试覆盖差距**

### 长期改进
1. **N2-N4, N6-N10**: 优化项
2. **P2 测试覆盖差距**
3. **线程安全测试**
4. **模糊测试**

---

## 📚 参考资源

- C++ Jsonnet: https://github.com/google/jsonnet
- go-jsonnet: https://github.com/google/go-jsonnet
- jrsonnet: https://github.com/CertainLach/jrsonnet
- Jsonnet 规范: https://jsonnet.org/ref/spec.html
- sjsonnet 测试套件: `test/resources/test_suite/`, `test/resources/go_test_suite/`

---

## 🔍 审查方法论

本次审查采用 PI 引擎 🐲深度模式，执行九令洞鉴：

1. **📖 读败**: 逐字读取所有相关源码
2. **🔍 主搜**: 搜索关键模式和引用
3. **📜 读典**: 对比 Jsonnet 规范和参考实现（go-jsonnet, jrsonnet, C++ Jsonnet）
4. **⚗️ 验假**: 验证每个假设
5. **🔄 反转**: 考虑反面情况
6. **🔻 缩域**: 缩小到最小复现
7. **🔀 换器**: 使用多种工具交叉验证
8. **👁️ 换位**: 从用户/上游/下游视角审视
9. **🌐 观局**: 判断是否为更大系统问题的表征

### 参考实现验证清单
- ✅ go-jsonnet `builtins.go`: 函数相等性、取模运算
- ✅ go-jsonnet `interpreter.go`: super 语义
- ✅ C++ Jsonnet `desugarer.cpp`: 运算符 desugar
- ✅ jrsonnet `evaluate/mod.rs`: super 语义
