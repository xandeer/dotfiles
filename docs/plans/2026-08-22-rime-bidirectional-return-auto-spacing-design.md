# Rime Return 双向英中自动空格设计

## 目标

补齐裸 `Return` 上屏纯英文字母时的 Han → ASCII 方向，同时保留已实现的 ASCII → Han 方向：

```text
中文候选 + harness + Return -> 中文 harness
harness + Return + 中文候选 -> harness 中文
```

## 根因

当前 Return processor 调用原生 `context:commit()` 上屏未加前缀的原始字母，之后才把最新历史标成 `return_raw`。该标记能影响下一次候选，却无法改写已经交给前端的本次文本；现有回归也没有记录 Return 实际提交内容。

## 范围

- 仅处理中文模式、正在组合、完整输入匹配 `[A-Za-z]+` 的无修饰 `Return`。
- 只有上一次可信 Rime 提交以 Han 结尾时，才为本次原始字母增加一个 U+0020。
- 普通 `raw`、`thru`、Ctrl+J/L、带修饰 Return、数字、标点和混合输入继续保持原样。
- 不读取宿主正文，不扩展粘贴、光标移动或外部编辑语义。

## 方案

继续复用首位 `select_character` processor 和现有 `committed_history_boundary()`、`boundary_codepoint()`、`needs_auto_space()`：

1. 在产生任何新提交前读取上一次可信边界。
2. 执行原生 `clear_non_confirmed_composition()`，并确认 `get_commit_text()` 仍精确等于捕获的纯字母输入。
3. 仅当边界为 Han → ASCII 时，先调用 `engine:commit_text(" ")`。
4. 始终继续调用原生 `context:commit()` 上屏字母，保留 commit notifier、候选学习和插件语义。librime Session 会累计同一次按键产生的 sink 文本，因此前端收到单个 `" harness"`。
5. 提交成功后，仍只把精确匹配 `{type = "raw", text = 原输入}` 的最新记录改成 `return_raw`，使下一次 Han 候选继续自动补空格。

所有可能抛错的边界证明和提交文本核验都在空格输出前完成。证明失败时不输出空格并回到原生 Return；字母已成功提交但历史标记失败时仍返回 `kAccepted`，避免重复提交。

## 未采用方案

- `engine:commit_text(" " .. input)` 后直接清空 context：虽然只有一次 sink，但绕过原生 commit notifier 和 composition-derived commit 语义。
- 修改 `context.input`：会触发重组和 update notifier，改变选择状态。
- 提交后修改历史文本：sink 已同步完成，不能修正当前前端文本。
- 放宽所有 `raw`：既不能修正本次输出，也会误纳 Ctrl+J/L 和其他 raw 来源。

## 验证

- 先让现有 processor harness 模拟真实时间线：提交前是中文候选历史，提交后是新的 raw 字母历史，并记录实际 commit 文本。
- RED 必须显示当前实现提交 `"harness"` 而不是 `" harness"`。
- GREEN 验证两个方向、恰好一个空格、最终 `return_raw`，并保留无历史、`raw`、`thru`、Ctrl+J/L 和非纯字母行为。
- 运行集中 Lua/AI 回归、zsh 语法、YAML 解析、原生临时构建和 `git diff --check`。
- fast-forward 合并后重新验证，安装到 `~/.cache/rime` 与 `~/Library/Rime`，执行 Squirrel `--reload`，再由真实输入确认双向结果。
