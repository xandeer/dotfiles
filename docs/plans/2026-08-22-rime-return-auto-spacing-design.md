# Rime Return 英中自动空格设计

## 目标

在中文模式中，纯英文字母通过裸 `Return` 原样上屏后，下一次选择中文候选时仍自动补一个半角空格：

```text
harness + Return + 中文候选 -> harness 中文
```

## 范围

- 仅处理正在组合的完整输入 `[A-Za-z]+`。
- 仅处理无修饰的 `Return`；`Ctrl+J/L`、带修饰 Return、ASCII 模式直通保持原样。
- 数字、标点、空白、混合输入以及普通 `raw`/`thru` 历史继续 fail closed。
- 不读取宿主正文，不覆盖粘贴、鼠标移动或外部编辑。

## 方案

复用 schema 中已经排在首位的 `select_character` processor。命中上述条件时，它执行 ExpressEditor 与裸 Return 相同的 `clear_non_confirmed_composition()` 和 `commit()` 流程；提交完成后，仅当最新历史精确等于 `{type = "raw", text = 原输入}` 时，把该记录类型改成专用的 `return_raw`。

现有 `committed_history_boundary()` 接受任意非空、非 `raw`、非 `thru` 类型，因此 `auto_space_filter` 和 `ai_candidate_filter` 会自然共享这个已证明来源，不需要放宽普通 `raw`，也不需要新增 schema 组件或跨提交状态。

如果提交前的检查或调用失败，processor 返回 `kNoop`，让原生 editor 继续处理。若提交已经成功但标记失败，则返回 `kAccepted`，避免同一次 Return 重复上屏；该次历史仍是 `raw`，后续保守地不补空格。

## 未采用方案

- 放开所有 ASCII 结尾的 `raw`：会同时纳入 `engine:commit_text()`、`Ctrl+J/L` 和未翻译尾部，无法证明来自 Return。
- 用 context property 暂存 Return 输入：需要额外清理和防碰撞，状态比直接标记刚产生的历史记录更宽。
- 修改 Squirrel 原生代码：能观察更完整的宿主提交，但超出这个 Rime 配置行为的范围。

## 验证

- 先扩展现有 Lua processor harness，证明裸 Return 提交并把精确匹配的 `raw` 标为 `return_raw`。
- 验证 `return_raw` ASCII -> Han 产生前导空格，并覆盖 AI 展示往返。
- 保留 `raw`、`thru`、`Ctrl+J/L`、非纯字母和异常路径的原样行为。
- 运行 Rime auto-space、AI 和配置集中回归、YAML 解析、部署构建及 `git diff --check`。
