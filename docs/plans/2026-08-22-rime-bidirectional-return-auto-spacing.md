# Rime Bidirectional Return Auto Spacing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让可信中文候选之后用裸 Return 上屏的纯英文字母带一个前导空格，同时保留字母 Return 之后中文候选的自动空格。

**Architecture:** 扩展现有首位 `select_character` processor：提交前复用 typed-history 边界判断，必要时先向同一次 Session commit 缓冲写入一个空格，再执行原生 `context:commit()`。最终 raw 字母记录继续只改类型为 `return_raw`；filter、AI 和 schema 不变。

**Tech Stack:** Rime/Squirrel 1.16、librime-lua、Lua 5.4、zsh、Ruby/Psych/Fiddle、Git。

---

### Task 1: 用真实 Return 时间线建立 RED

**Files:**
- Modify: `tests/config/rime-auto-space-regression.lua:933-1075`

**Step 1: Update the existing harness**

让 `run_return_commit()` 区分提交前的 `previous_record` 和提交后生成的 raw record，并记录 `engine:commit_text()` 与 `context:commit()` 的实际 sink 文本。`context:get_commit_text()` 默认返回捕获的纯字母输入；提交成功后才让 `commit_history:back()` 返回最终 record。

核心断言：

```lua
local han_then_return = run_return_commit("harness", {
    previous_record = history_record("phrase", "中文"),
})
same(table.concat(han_then_return.commits), " harness",
    "Han then Return committed text")
same(han_then_return.record.type, "return_raw",
    "Han then Return history type")
same(run_auto_filter(han_then_return.record, {
    candidate("phrase", 0, 2, "中文"),
})[1].text, " 中文", "Return letters to future Han spacing")
```

增加两个窄负例：上一次记录分别为 `raw` 和 `thru`、文本为 `中` 时，只提交 `harness`。无历史、非纯字母和现有 Ctrl+J/L 断言保持。

**Step 2: Run test to verify it fails**

Run:

```nu
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: FAIL，当前实现实际提交 `harness`，期望 ` harness`。

### Task 2: 在原生 Return 提交前补空格

**Files:**
- Modify: `rime/rime.lua:372-398`

**Step 1: Write minimal implementation**

在纯字母校验之后、产生新历史之前保存左边界：

```lua
local left = committed_history_boundary(env)
context:clear_non_confirmed_composition()
local native_text = context:get_commit_text()
if native_text == input and needs_auto_space(
        left, boundary_codepoint(input, false)) then
    engine:commit_text(" ")
end
if context:commit() ~= true then
    return
end
```

保留现有 `committed = true` 和精确 `{raw, input} -> return_raw` 标记顺序。所有证明读取必须位于 `engine:commit_text(" ")` 之前；无可信边界或 native text 失配时仍只走原生提交。

**Step 2: Run test to verify it passes**

Run:

```nu
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected:

```text
Rime auto-space regression OK
Rime AI regression OK
```

**Step 3: Commit**

```nu
git add rime/rime.lua tests/config/rime-auto-space-regression.lua
git commit -m "fix(rime): space Return letters after Han"
```

### Task 3: Verify, merge, and deploy

**Files:**
- Verify: `rime/rime.lua`
- Verify: `tests/config/rime-auto-space-regression.lua`

**Step 1: Run focused gates**

```nu
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
ruby -ryaml -e 'ARGV.each { |path| YAML.load_file(path) }' rime/double_pinyin_flypy.schema.yaml rime/darwin/squirrel.custom.yaml
git diff --check master..HEAD
```

**Step 2: Native temporary build**

复用主 checkout 中被忽略的 grammar/词典文件，在 worktree 建立 ignored symlink；安装到 `mktemp -d` 创建的临时 HOME，并运行 bundled `rime_deployer --build`。要求 deployer 退出 0、`2 success, 0 failure`、存在 `double_pinyin_flypy` 产物且无 Lua/component fatal error。

**Step 3: Review and integrate**

只读审核确认：空格只在可信 Han → 纯 ASCII 字母 Return 边界产生；Session sink 顺序为 `" "` 后原生字母；最终历史仍为 `return_raw`；普通 `raw`/`thru`、Ctrl+J/L 和失败路径不扩展。

fast-forward 合并到 `master`，在合并结果重跑 Step 1，然后执行：

```nu
make -C rime install
'/Library/Input Methods/Squirrel.app/Contents/MacOS/Squirrel' --reload
```

验证仓库、`~/Library/Rime/rime.lua`、`~/.cache/rime/rime.lua` 哈希一致，再清理功能 worktree/branch。真实验收目标为 `中文 harness` 与 `harness 中文`。
