# Evaluator.scala 深度审查发现

## Blocker 级别问题

### 1. `evalBinaryOpNumNum` 中 `%` 运算符缺少除零检查 (NaN 泄漏)

**文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:722`
**问题**: `evalBinaryOpNumNum` 方法中 `OP_%` 没有检查除数是否为零。`ld % rd` 当 `rd == 0` 时在 IEEE 754 中产生 `NaN`。`Val.Num` 构造函数 (line 471) 只检查 `isInfinite`，不检查 `isNaN`，因此 NaN 会泄漏到系统中。
**影响**: 在 comprehension 快速路径中，`[1 % 0 for x in [1]]` 不会抛出错误，而是静默产生 NaN 值。NaN 会传播到后续操作中，导致不可预测的行为。
**参考实现**:
- go-jsonnet: 统一抛出 "Division by zero."
- C++ reference: 统一抛出 "division by zero."
- jrsonnet: 统一抛出 DivisionByZero

**修复**: 在 line 722 的 `OP_%` 分支中添加 `if (rd == 0) Error.fail("Division by zero.", pos)`

### 2. `visitBinaryOpAsDouble` 中 `%` 运算符缺少除零检查 (NaN 泄漏)

**文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:864-865`
**问题**: `visitBinaryOpAsDouble` 方法中 `OP_%` 没有检查除数是否为零。当 `%` 的结果作为另一个算术运算的输入时（如 `a + (1 % 0)`），`visitExprAsDouble` 会调用此路径，NaN 会传播到父运算中。
**影响**: `a + (1 % 0)` 不会抛出 "Division by zero."，而是静默产生 NaN，进而传播到整个表达式链。
**参考实现**: 同上

**修复**: 在 line 865 中添加除零检查：
```scala
case Expr.BinaryOp.OP_% =>
  val l = visitExprAsDouble(e.lhs)
  val r = visitExprAsDouble(e.rhs)
  if (r == 0) Error.fail("Division by zero.", pos)
  l % r
```

## 非 Blocker 但值得注意的问题

### 3. `/` 运算符错误消息与 `%` 不一致

**文件**: `sjsonnet/src/sjsonnet/Evaluator.scala:1334` vs `1346`
**问题**: `OP_/` 使用 "division by zero" (小写，无句号)，`OP_%` 使用 "Division by zero." (大写，有句号)
**影响**: 仅影响错误消息一致性，不影响语义正确性。同一实现内不一致。
**参考实现**: C++ 使用 "division by zero."，go-jsonnet 使用 "Division by zero."
