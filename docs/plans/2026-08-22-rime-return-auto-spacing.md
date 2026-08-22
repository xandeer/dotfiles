# Rime Return Auto Spacing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让中文模式中用裸 Return 原样上屏的纯英文字母，成为下一次英中自动空格的可信左边界。

**Architecture:** 复用首位 `select_character` processor，在提交发生的同步边界执行原生 Return 流程并把精确匹配的最新 `raw` 历史改标为 `return_raw`。现有两个 filter 已接受非保留历史类型，因此不修改 filter 或 schema；普通 `raw`/`thru` 继续排除。

**Tech Stack:** Rime/Squirrel 1.16、librime-lua、Lua 5.4、zsh、Ruby/Psych/Fiddle、Git。

---

### Task 1: 用回归锁定 Return 来源

**Files:**
- Modify: `tests/config/rime-auto-space-regression.lua:891-930`
- Modify: `tests/config/rime-ai-regression.lua:548-665`

**Step 1: Write the failing test**

在现有 processor harness 旁增加 `run_return_commit(input, options)`，模拟 `context.input`、`ascii_mode`、composition、原生 commit 和可写 `commit_history:back()`。断言：

```lua
local returned = run_return_commit("harness")
same(returned.result, 1, "plain Return commit return value")
same(returned.record.type, "return_raw", "plain Return history type")
same(run_auto_filter(returned.record, {
    candidate("phrase", 0, 2, "中文"),
})[1].text, " 中文", "plain Return ASCII to Han spacing")
```

同时覆盖 ASCII 模式、非 composing、非纯字母、commit 返回 false、历史失配及标记写入异常。AI harness 增加 `return_raw` ASCII 历史可反解单个展示空格的契约，同时保留普通 `raw`/`thru` negative。

**Step 2: Run test to verify it fails**

Run:

```nu
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: FAIL，裸 Return 仍返回 `kNoop`，没有生成 `return_raw`。

### Task 2: 在现有 processor 中做最小实现

**Files:**
- Modify: `rime/rime.lua:367-409`

**Step 1: Write minimal implementation**

在读取 Ctrl+J/L 配置前处理严格的 `key:repr() == "Return"`：

```lua
local committed = false
pcall(function()
    if context:get_option("ascii_mode") or
        context:is_composing() ~= true then
        return
    end
    local input = context.input
    if type(input) ~= "string" or input:match("^[A-Za-z]+$") == nil then
        return
    end

    context:clear_non_confirmed_composition()
    if context:commit() ~= true then
        return
    end
    committed = true

    local record = context.commit_history:back()
    if record ~= nil and record.type == "raw" and record.text == input then
        record.type = "return_raw"
    end
end)
return committed and 1 or 2
```

`committed` 必须在历史读取前置为 true：提交后的标记失败仍返回 `kAccepted`，避免原生 editor 再次上屏。

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
git add rime/rime.lua tests/config/rime-auto-space-regression.lua tests/config/rime-ai-regression.lua
git commit -m "feat(rime): space after Return letters"
```

### Task 3: 聚焦审核与原生构建

**Files:**
- Verify: `rime/rime.lua`
- Verify: `tests/config/rime-auto-space-regression.lua`
- Verify: `tests/config/rime-ai-regression.lua`

**Step 1: Run focused gates**

```nu
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
ruby -ryaml -e 'ARGV.each { |path| YAML.load_file(path) }' rime/double_pinyin_flypy.schema.yaml rime/darwin/squirrel.custom.yaml
git diff --check (git merge-base master HEAD)..HEAD
```

Expected: 两个 harness 打印 `OK`，其余命令退出 0 且无 diff-check 输出。

**Step 2: Build in a temporary HOME**

从主 checkout 只读复用被忽略的 grammar/词典文件，在 worktree 建立同样被忽略的精确 symlink；随后用 `mktemp -d` 创建临时 HOME，运行：

```nu
make -C rime HOME=$native_root install
with-env {GLOG_logtostderr: 1} {
    '/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_deployer' --build $rime_root '/Library/Input Methods/Squirrel.app/Contents/SharedSupport' $build_root
}
```

Expected: deployer 退出 0，build 中存在 `double_pinyin_flypy` 产物，日志没有 Lua/component fatal error。

**Step 3: Review and stop**

对照设计检查：仅裸 Return + `[A-Za-z]+` 被标记；普通 `raw`/`thru`、Ctrl+J/L 和所有失败路径未扩展。重新运行 Task 3 Step 1，并报告自动化证据与真实输入尚未覆盖的边界。
