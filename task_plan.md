# Evaluator.scala 深度审查任务计划

## 目标
深度审查 sjsonnet 的 Evaluator.scala 实现的正确性，找出 blocker 级别的问题。

## 验收标准
- [ ] 完整读取 Evaluator.scala
- [ ] 对比 go-jsonnet、jrsonnet、jsonnet 参考实现
- [ ] 识别所有 blocker 级别的正确性问题
- [ ] 每个问题包含文件路径、行号、问题描述、参考实现行为、影响程度

## 阶段

### 阶段 1: 读取 Evaluator.scala
- 状态: 进行中
- 任务: 读取完整内容，理解整体结构

### 阶段 2: 分析关键区域
- 状态: 待开始
- 任务: 重点分析二元运算、比较运算、函数调用、import、对象/数组操作

### 阶段 3: 对比参考实现
- 状态: 待开始
- 任务: 查看 go-jsonnet、jrsonnet、jsonnet 的对应实现

### 阶段 4: 记录问题
- 状态: 待开始
- 任务: 整理所有 blocker 级别的问题

## 进度日志
- 2026-06-10: 任务开始
