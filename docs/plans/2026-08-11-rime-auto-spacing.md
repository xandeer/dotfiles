# Rime Automatic Chinese-English Spacing Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在中文模式的连续 Rime 候选提交之间，仅对 Han ↔ ASCII 字母边界自动补一个半角空格，同时保持数字、标点、已有空白、AI 学习和用户词典语义不变。

**Architecture:** 新增最终 `auto_space_filter`，恰好一次读取 `commit_history:back()` 的 `type/text`，只接受非空且非 `thru`/`raw` 的候选提交记录，并装饰 `candidate.start == 0` 的最终显示候选。过滤器运行在 `uniquifier` 之后，通过 `get_dynamic_type()` 区分 `Shadow`/`Uniquified` wrapper 与 `Sentence`/`Phrase`/`Simple`/`Other` 终态，再有界、带循环检测地归一到最底层 genuine candidate，并只增加一层 `ShadowCandidate`；无法无损证明 typed history、dynamic type、genuine、span 或 comment 时原样通过。`select_character` 从 composition 的首 segment 识别 `auto_space` 类型并在 `Ctrl+J/L` 提交中保留唯一自动前缀。

**Tech Stack:** Rime/Squirrel 1.16、librime-lua、Lua 5.4 `utf8`、YAML、zsh、Ruby/Psych/Fiddle、GNU Make、Git。

---

## 执行边界

- 在隔离 worktree `/Users/kevin/projects/personal/dotfiles/.worktrees/rime-auto-spacing` 中执行。
- 不修改主 checkout 中用户已有的 `config/.codex/config.toml`。
- Tasks 1–6 不写入 `/Users/kevin/Library/Rime`；只使用仓库、`/tmp` 与临时 HOME。
- Task 7 会覆盖当前用户的 Rime 配置并重载 Squirrel，必须先取得用户单独授权。
- 任一意外失败先使用 `superpowers:systematic-debugging` 找根因，不通过削弱断言获得绿色。
- 本设计与计划在开始 Task 1 前必须已被 Git 跟踪并提交；用 `git ls-files --error-unmatch` 检查两个文档，不能把未跟踪计划留到 Task 5。
- 多命令 zsh block 必须以 `set -eu` 开始，或逐条运行并在首个非零退出码处停止；禁止让后续成功命令掩盖前序失败。

开始实现前运行：

```zsh
set -eu
git ls-files --error-unmatch \
  docs/plans/2026-08-11-rime-auto-spacing-design.md \
  docs/plans/2026-08-11-rime-auto-spacing.md
[[ -z "$(git status --porcelain -- \
  docs/plans/2026-08-11-rime-auto-spacing-design.md \
  docs/plans/2026-08-11-rime-auto-spacing.md)" ]]
```

## Task 1: 建立 filter 与学习契约的 RED 回归

**Files:**

- Create: `tests/config/rime-auto-space-regression.lua`
- Modify: `tests/config/rime-config-regression.zsh`
- Modify: `tests/config/rime-ai-regression.lua`
- Test: `tests/config/rime-auto-space-regression.lua`
- Test: `tests/config/rime-ai-regression.lua`

### Step 1: 新建单层候选 mock

在 `tests/config/rime-auto-space-regression.lua` 开头先锁定生产入口：

```lua
assert(type(auto_space_filter) == "function", "missing production auto_space_filter")

local function same(actual, expected, message)
    assert(actual == expected, string.format(
        "%s: expected %q, got %q",
        message or "values differ",
        tostring(expected),
        tostring(actual)
    ))
end

local function candidate(kind, start_pos, end_pos, text, comment, quality, dynamic_type)
    local native_dynamic_type = dynamic_type or "Simple"
    local value = {
        type = kind,
        start = start_pos,
        _end = end_pos,
        text = text,
        comment = comment or "",
        quality = quality or 0,
    }
    function value:get_genuine()
        return self
    end
    function value:get_dynamic_type()
        return native_dynamic_type
    end
    return value
end

local function wrapper(dynamic_type, kind, text, comment, source, start_pos, end_pos)
    local value = {
        type = kind,
        start = start_pos or source.start,
        _end = end_pos or source._end,
        text = text,
        comment = comment or "",
        quality = source.quality,
    }
    function value:get_genuine()
        return source
    end
    function value:get_dynamic_type()
        return dynamic_type
    end
    return value
end
```

Wrapper mock 的 `get_genuine()` 每次只解一层，不能递归返回 ultimate，否则会掩盖生产代码的嵌套 Shadow 缺陷。`dynamic_type` 必须独立于逻辑候选 `type`，wrapper 分别使用 `Shadow` 或 `Uniquified`，base 默认使用终态 `Simple`/`Phrase`。

librime-lua 每次返回 C++ `shared_ptr<Candidate>` 都创建新的 Lua userdata，且 Candidate metatable 没有 `__eq`，所以生产代码不能用 `==` 或 `rawequal` 判断 genuine 固定点。增加一个 native-like 终态 fixture：它的 `get_dynamic_type()` 返回终态，但 `get_genuine()` 返回新的终态 alias（另一个 fixture 可抛错）；断言 filter 完全不调用终态的 `get_genuine()`。

再实现：

- `ShadowCandidate(source, kind, text, comment)`，记录每次构造参数并返回一层 dynamic type 为 `Shadow` 的 `wrapper`；
- `stream(values)`，返回与现有 AI harness 相同的 `(iterator, state)`；
- `yield(value)`，记录输出顺序；
- `commit_history:back()` 返回带 `type/text` 的记录，`run_auto_filter(history, candidates)` 统计其恰好读取一次；
- `commit_history:latest_text()` 的 mock 必须抛错或返回误导边界，并断言生产 filter 从不读取它；
- 可构造固定深度链、循环、抛错和 `nil` 返回的 genuine fixtures。

### Step 2: 写边界矩阵

为以下正例断言输出文本（历史均使用非 `thru`/`raw` 的候选记录）：

```text
中文 + Rime -> " Rime"
中文 + rime -> " rime"
Rime + 输入法 -> " 输入法"
rime + 输入法 -> " 输入法"
```

另以 `A/Z/a/z` 冻结 ASCII 大小写字母的两个端点，并以 `@/[`、反引号、`{` 冻结相邻非字母，两种边界方向均覆盖。

逐一覆盖这些 Han 范围的两个端点：

```text
U+3400–U+4DBF
U+4E00–U+9FFF
U+F900–U+FAFF
U+20000–U+2FA1F
U+30000–U+3347F
```

逐一覆盖紧邻范围外的码点，并断言原对象 identity 保持不变。还要覆盖：

- `第 + 3次`、`2026 + 年`；
- 标点、Emoji、已有历史空格、候选前导空格；
- 空历史、空候选、历史或候选非法 UTF-8；
- `candidate.start ~= 0`；
- 已有 `type == "auto_space"`；
- 候选数量与顺序不变；
- 默认历史使用合理的候选记录类型，任意非空非保留类型仍触发规则；
- 直接 ASCII 记录 `thru` 和 Han↔ASCII 两方向的 `raw` 记录都原样通过；
- `back()` 抛错、返回空记录，或记录 `type/text` 访问抛错/值非法时 fail closed；
- 每次 filter 调用恰好读取一次 `back()`，且绝不读取 `latest_text()`。

### Step 3: 写 ultimate genuine 与 fail-closed 矩阵

构造最终 display 与 genuine 元数据故意不同的链：

```lua
local ultimate = candidate(
    "ai_learned", 0, 4, "ultimate-text", "ultimate-comment", 1.7
)
local simplified = wrapper(
    "Shadow", "simplified", "intermediate-text", "intermediate-comment", ultimate
)
local final = wrapper(
    "Uniquified", "uniquified", "Rime Display", "final-comment", simplified
)

local output = run_auto_filter("中文", {final})[1]
local call = shadow_calls[1]

same(call.source, ultimate, "ShadowCandidate source must be ultimate genuine")
same(call.kind, "auto_space", "ShadowCandidate type")
same(call.text, " Rime Display", "spacing must use final display text")
same(call.comment, "final-comment", "spacing must use final display comment")
assert(rawequal(output:get_genuine(), ultimate),
    "final ShadowCandidate must expose ultimate genuine")
```

另断言边界分类使用 final display，而不是 ultimate text：

- ultimate 以 Han 开头、final display 以 ASCII 开头；
- ultimate 以 ASCII 开头、final display 以 Han 开头。

冻结最多 16 次 wrapper transition 的契约：16 层成功，17 层原样通过。以下情况均不得抛错且必须返回原对象：

- `get_dynamic_type()` 抛错、返回 `nil`、非字符串或未知类型；
- genuine 链循环；
- `get_genuine()` 抛错、返回 `nil` 或非候选；
- genuine 与 final 的 `start/_end` 不同；
- final comment 为空但 genuine comment 非空；
- `ShadowCandidate` 构造抛错或返回 `nil`。

### Step 4: 把 harness 接入 bundled Lua，且隔离 Lua state

在 `tests/config/rime-config-regression.zsh` 中增加：

```zsh
auto_space_harness="$repo_root/tests/config/rime-auto-space-regression.lua"
```

把它加入文件存在检查，并把 Ruby/Fiddle 调用改为：

```zsh
ruby -rfiddle - \
  "$repo_root/rime/rime.lua" \
  "$auto_space_harness" \
  "$ai_harness"
```

Ruby 端使用 `production, *harnesses = ARGV`，为每个 harness 分别执行：

1. `luaL_newstate`；
2. `luaL_openlibs`；
3. 加载生产 `rime.lua`；
4. 加载一个 harness；
5. `ensure` 中 `lua_close`。

不要在两个 harness 间复用 `Candidate`、`ShadowCandidate`、`yield`、`rime_api` 或 I/O monkeypatch。

### Step 5: 把 AI 学习测试接到生产 filter 输出

在 `tests/config/rime-ai-regression.lua` 中加入同样的单层 `ShadowCandidate` mock 和 `run_auto_filter` helper。在现有第二次 `ai_learned` 增量断言之后新增一个 `start == 0` 场景；该条件必须满足生产 filter 的首 segment 契约：

```lua
learn_context.input = "code"
learn_context.composition.segment = {start = 0, _end = 4, status = "selected"}
local ultimate = candidate("ai_learned", 0, 4, "spaced correction")
local final = wrapper(
    "Uniquified", "uniquified", "Chosen Display", "final-comment", ultimate
)
local spaced = run_auto_filter("中文", {final})[1]

same(spaced.type, "auto_space", "AI integration must exercise auto spacing")
same(spaced.text, " Chosen Display", "AI integration spaced display")
assert(rawequal(spaced:get_genuine(), ultimate),
    "AI integration must expose ultimate genuine")

learn_context.selected_candidate = spaced
learn_context.properties._ai_generation = ""
learn_context.select_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
```

断言 TSV 只包含 ultimate genuine 文本：

```lua
local count, weight = exact_rows(
    read_file(weights_path), "test_schema", "code", "spaced correction"
)
same(count, 1, "AI learning must persist ultimate genuine text")
same(weight, 1, "AI learning ultimate genuine weight")

local spaced_count = exact_rows(
    read_file(weights_path), "test_schema", "code", " Chosen Display"
)
same(spaced_count, 0, "spaced display text must not enter AI learning")
```

保持原有 0600 权限、取消选择、重复 commit 和故障注入断言不变。

### Step 6: 运行 RED

Run:

```zsh
set -eu
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: syntax check succeeds; regression fails with:

```text
rime-auto-space-regression.lua: missing production auto_space_filter
```

不要在 RED 状态提交。

首次实现后的 native binding 审查确认：`get_genuine()` 即使返回同一个 C++ candidate，也会生成新的 Lua userdata，因此旧的 Lua identity 固定点永远不能成立。先把终态 mock 改为每次 `get_genuine()` 返回 fresh alias，并在修改生产解包逻辑前捕获第二个 RED：

```text
terminal dynamic type must resolve without calling get_genuine: expected "0", got "17"
```

## Task 2: 实现最终候选自动空格 filter

**Files:**

- Modify: `rime/rime.lua`
- Modify: `docs/plans/2026-08-11-rime-auto-spacing-design.md`
- Modify: `docs/plans/2026-08-11-rime-auto-spacing.md`
- Test: `tests/config/rime-auto-space-regression.lua`
- Test: `tests/config/rime-ai-regression.lua`

### Step 1: 增加安全 Unicode 边界 helper

在 `rime/rime.lua` 的 UTF-8 helper 附近增加：

```lua
local function boundary_codepoint(text, from_end)
    if type(text) ~= "string" or text == "" then
        return nil
    end
    local offset_ok, offset = pcall(utf8.offset, text, from_end and -1 or 1)
    if not offset_ok or not offset then
        return nil
    end
    local code_ok, codepoint = pcall(utf8.codepoint, text, offset, offset)
    return code_ok and codepoint or nil
end

local function is_ascii_letter(codepoint)
    return codepoint and
        ((codepoint >= 0x41 and codepoint <= 0x5A) or
         (codepoint >= 0x61 and codepoint <= 0x7A))
end

local function is_han(codepoint)
    return codepoint and
        ((codepoint >= 0x3400 and codepoint <= 0x4DBF) or
         (codepoint >= 0x4E00 and codepoint <= 0x9FFF) or
         (codepoint >= 0xF900 and codepoint <= 0xFAFF) or
         (codepoint >= 0x20000 and codepoint <= 0x2FA1F) or
         (codepoint >= 0x30000 and codepoint <= 0x3347F))
end

local function needs_auto_space(left, right)
    return (is_han(left) and is_ascii_letter(right)) or
        (is_ascii_letter(left) and is_han(right))
end
```

### Step 2: 增加有界 genuine 归一

实现最多 16 次 wrapper transition。由于 librime-lua 会为同一个 C++ candidate 创建新的 userdata，禁止用 Lua identity 证明固定点；改为白名单 dynamic type，终态直接返回，只有 `Shadow`/`Uniquified` 才调用 `get_genuine()`。`seen` 仍用于 mock identity 循环检测，深度上限处理 native alias 无法用 identity 检测的防御边界：

```lua
local terminal_candidate_types = {
    Sentence = true,
    Phrase = true,
    Simple = true,
    Other = true,
}

local wrapper_candidate_types = {
    Shadow = true,
    Uniquified = true,
}

local function ultimate_genuine(candidate)
    local current = candidate
    local seen = {}

    for depth = 0, 16 do
        local current_type = type(current)
        if (current_type ~= "table" and current_type ~= "userdata") or seen[current] then
            return nil
        end
        seen[current] = true

        local dynamic_ok, dynamic_type = pcall(function()
            return current:get_dynamic_type()
        end)
        if not dynamic_ok or type(dynamic_type) ~= "string" then
            return nil
        end
        if terminal_candidate_types[dynamic_type] then
            return current
        end
        if not wrapper_candidate_types[dynamic_type] or depth == 16 then
            return nil
        end

        local genuine_ok, next_candidate = pcall(function()
            return current:get_genuine()
        end)
        local next_type = type(next_candidate)
        if not genuine_ok or
            (next_type ~= "table" and next_type ~= "userdata") then
            return nil
        end
        current = next_candidate
    end

    return nil
end
```

增加 `same_candidate_span(final, genuine)`，要求两端的数值 `start/_end` 均存在并完全相等：

```lua
local function same_candidate_span(final, genuine)
    local final_start = tonumber(final.start)
    local final_end = tonumber(final._end)
    local genuine_start = tonumber(genuine.start)
    local genuine_end = tonumber(genuine._end)
    return final_start ~= nil and final_end ~= nil and
        genuine_start ~= nil and genuine_end ~= nil and
        final_start == genuine_start and final_end == genuine_end
end
```

### Step 3: 实现 filter 并 fail closed

生产函数按此数据流实现：

```lua
local function committed_history_boundary(env)
    local history_ok, record_type, record_text = pcall(function()
        local record = env.engine.context.commit_history:back()
        if record == nil then
            return nil, nil
        end
        return record.type, record.text
    end)
    if not history_ok or type(record_type) ~= "string" or record_type == "" or
        record_type == "thru" or record_type == "raw" or
        type(record_text) ~= "string" or record_text == "" then
        return nil
    end
    return boundary_codepoint(record_text, true)
end

function auto_space_filter(input, env)
    local left = committed_history_boundary(env)

    for candidate in input:iter() do
        local text = type(candidate.text) == "string" and candidate.text or ""
        local display_comment = type(candidate.comment) == "string" and
            candidate.comment or ""
        local right = boundary_codepoint(text, false)
        local should_wrap = tonumber(candidate.start) == 0 and
            tostring(candidate.type or "") ~= "auto_space" and
            needs_auto_space(left, right)

        if should_wrap then
            local genuine = ultimate_genuine(candidate)
            local genuine_comment = genuine and
                type(genuine.comment) == "string" and genuine.comment or ""

            if genuine and same_candidate_span(candidate, genuine) and
                not (display_comment == "" and genuine_comment ~= "") then
                local wrap_ok, wrapped = pcall(
                    ShadowCandidate,
                    genuine,
                    "auto_space",
                    " " .. text,
                    display_comment
                )
                if wrap_ok and wrapped then
                    yield(wrapped)
                else
                    yield(candidate)
                end
            else
                yield(candidate)
            end
        else
            yield(candidate)
        end
    end
end
```

不要回退读取 `latest_text()`，因为它只保留 `back().text`、会丢失 `thru`/`raw` 来源。不要传或依赖第 5 个 `inherit_comment` 参数；当前 librime-lua 接收但不透传它。

### Step 4: 运行 GREEN

Run:

```zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected:

```text
Rime auto-space regression OK
Rime AI regression OK
```

### Step 5: 提交 filter 与回归

Run:

```zsh
set -eu
git diff --check
git add docs/plans/2026-08-11-rime-auto-spacing-design.md \
  docs/plans/2026-08-11-rime-auto-spacing.md \
  rime/rime.lua \
  tests/config/rime-auto-space-regression.lua \
  tests/config/rime-ai-regression.lua \
  tests/config/rime-config-regression.zsh
git diff --cached --check
git commit -m "feat(rime): add automatic spacing filter"
```

## Task 3: 以 RED/GREEN 注册最终 filter 顺序

**Files:**

- Modify: `tests/config/rime-config-regression.zsh`
- Modify: `rime/double_pinyin_flypy.schema.yaml`

### Step 1: 先收紧配置断言

把当前两项相邻断言替换为严格、唯一的三项链：

```ruby
ai_name = "lua_filter@ai_candidate_filter"
unique_name = "uniquifier"
space_name = "lua_filter@auto_space_filter"

ai_index = filters.index(ai_name)
unique_index = filters.index(unique_name)
space_index = filters.index(space_name)

unless filters.count(ai_name) == 1 &&
    filters.count(unique_name) == 1 &&
    filters.count(space_name) == 1 &&
    unique_index == ai_index.to_i + 1 &&
    space_index == unique_index.to_i + 1
  abort "expected filters to contain exactly ai_candidate_filter -> uniquifier -> auto_space_filter"
end
```

### Step 2: 验证 schema RED

Run:

```zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected:

```text
expected filters to contain exactly ai_candidate_filter -> uniquifier -> auto_space_filter
```

### Step 3: 注册最终 filter

把 schema 调整为：

```yaml
  filters:
    - simplifier@emoji
    - simplifier@traditionalize
    - lua_filter@ai_candidate_filter
    - uniquifier
    - lua_filter@auto_space_filter
```

### Step 4: 验证 GREEN 并提交

Run:

```zsh
set -eu
/bin/zsh tests/config/rime-config-regression.zsh
git diff --check
git add rime/double_pinyin_flypy.schema.yaml \
  tests/config/rime-config-regression.zsh
git diff --cached --check
git commit -m "feat(rime): register automatic spacing filter"
```

Expected harness output仍为：

```text
Rime auto-space regression OK
Rime AI regression OK
```

## Task 4: 以 RED/GREEN 保持 `Ctrl+J/L` 自动前缀

**Files:**

- Modify: `tests/config/rime-auto-space-regression.lua`
- Modify: `rime/rime.lua`

### Step 1: 增加 processor 与 segmentation mock

扩展 harness：

- `composition:empty()` 与 `composition:toSegmentation()`；
- segmentation 的 `empty()`、`size`、零基 `get_at(index)`；
- segment 的 `get_selected_candidate()`；
- engine 记录 `commit_text(text)`；
- context 记录且计数 `clear()`；
- key/config mock 提供 `Control+j` 与 `Control+l`。

### Step 2: 写正负例

单 segment，display 与 genuine 故意不同：

```text
Control+j + " Rime" -> " R"
Control+l + " Rime" -> " e"
```

两 segment、当前活动 segment 为第二段，但首段选中 `auto_space`：

```text
Control+j + " Rime输入法" -> " R"
Control+l + " Rime输入法" -> " 法"
```

每个 accepted 分支恰好调用一次 `context:clear()`。以下情况保持旧逻辑：

- 相同显示文本但 type 不是 `auto_space`；
- type 正确但完整 commit 不以前缀显示文本开头；
- 0 个或多于 1 个前导空格；
- 首 segment 无选中候选；
- genuine 不存在或 `get_genuine()` 抛错；
- composition/segmentation API 缺失或抛错。

### Step 3: 运行 processor RED

Run:

```zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected representative failures:

```text
Ctrl+J must preserve automatic prefix: expected " R", got " "
Ctrl+L must preserve automatic prefix: expected " e", got "e"
multi-segment Ctrl+L must preserve automatic prefix: expected " 法", got "法"
```

### Step 4: 实现首 segment 自动前缀证明

新增一个 fail-closed helper。核心判定必须同时满足：

```lua
tostring(selected.type or "") == "auto_space"
display:sub(1, 1) == " "
display:sub(2, 2) ~= " "
commit_text:sub(1, #display) == display
```

并通过 `pcall` 证明：

```lua
context.composition:toSegmentation():get_at(0)
first_segment:get_selected_candidate()
selected:get_genuine()
```

任一失败返回 `""`。成功时返回唯一前缀 `" "`。

在 `select_character` 中改为：

```lua
local prefix = auto_space_prefix(context, commit_text)
local selectable_text = prefix == "" and commit_text or commit_text:sub(2)

if key:repr() == first_key and selectable_text ~= "" then
    engine:commit_text(prefix .. first_character(selectable_text))
    context:clear()
    return 1
end

if key:repr() == last_key and selectable_text ~= "" then
    engine:commit_text(prefix .. last_character(selectable_text))
    context:clear()
    return 1
end
```

不要要求 `display == " " .. genuine.text`；简繁 display 可以与 genuine 不同。

保守边界：当前 `select_character` 用 `engine:commit_text()` 提交 `Ctrl+J/L` 结果，librime 将它记为 `raw`。因此后续候选故意不自动补空格。若 Task 4 或未来变更要保留这种跨提交空格，必须显式保留可验证的候选来源；不得放宽 Task 1–2 的 `raw` 排除。

### Step 5: 验证 GREEN 并提交

Run:

```zsh
set -eu
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
git diff --check
git add rime/rime.lua tests/config/rime-auto-space-regression.lua
git diff --cached --check
git commit -m "fix(rime): preserve spacing for character selection"
```

## Task 5: 审核实现差异与回归覆盖

**Files:**

- Review: `rime/rime.lua`
- Review: `rime/double_pinyin_flypy.schema.yaml`
- Review: `tests/config/rime-auto-space-regression.lua`
- Review: `tests/config/rime-ai-regression.lua`
- Review: `tests/config/rime-config-regression.zsh`

### Step 1: 运行集中自动化验证

Run:

```zsh
set -eu
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
ruby -ryaml -e 'ARGV.each { |path| YAML.load_file(path) }' \
  rime/double_pinyin_flypy.schema.yaml \
  rime/darwin/squirrel.custom.yaml
feature_base="$(git merge-base master HEAD)"
git diff --check "$feature_base"..HEAD
git diff --name-status "$feature_base"..HEAD
git log --oneline "$feature_base"..HEAD
git status --short --branch
```

Expected:

- 两个 harness 都打印 `OK`；
- Psych 无异常；
- branch-range `git diff --check` 无输出；
- branch-range 文件清单只包含设计、计划和自动空格实现/测试，不包含主 checkout 的 `config/.codex/config.toml`；
- worktree status 干净，且能看到本分支相对 fork point 的全部提交。

### Step 2: 使用 `superpowers:requesting-code-review`

请求 reviewer 对照设计文档检查：

- filter 确实位于 `uniquifier` 后；
- dynamic type 异常/未知，以及 genuine 循环/超深/span/comment 冲突全部原样通过；
- wrapper source 是 ultimate，display/comment 来自 final；
- AI TSV 永不保存前导空格；
- `select_character` 使用首 segment，而不是活动末 segment；
- 没有把 mock 或 deploy 结果表述为真实用户词典证明。

如有反馈，在改动前使用 `superpowers:receiving-code-review` 验证；每个修正后重跑集中验证。

## Task 6: 临时 HOME 原生部署验证

**Files:**

- Verify: `rime/rime.lua`
- Verify: `rime/double_pinyin_flypy.schema.yaml`
- Verify: `/tmp/rime-auto-space-native.*`

### Step 1: 安全补齐被 Git 忽略的本地依赖

feature worktree 不包含被忽略的语言模型和本地解密词典。只检查路径、大小、忽略状态和 symlink 目标；禁止打印词典内容：

```zsh
set -eu
main_checkout="/Users/kevin/projects/personal/dotfiles"
feature_worktree="/Users/kevin/projects/personal/dotfiles/.worktrees/rime-auto-spacing"
grammar_source="$main_checkout/rime/amz-v2n3m1-zh-hans.gram"
xandeer_source="$main_checkout/etc/xandeer.dict.yaml"
grammar_overlay="$feature_worktree/rime/amz-v2n3m1-zh-hans.gram"
xandeer_overlay="$feature_worktree/etc/xandeer.dict.yaml"

[[ -s "$grammar_source" && -s "$xandeer_source" ]] || {
  print -u2 "missing required ignored Rime dependency"
  exit 1
}
git -C "$main_checkout" check-ignore -q rime/amz-v2n3m1-zh-hans.gram
git -C "$main_checkout" check-ignore -q etc/xandeer.dict.yaml

mkdir -p "$feature_worktree/etc"
[[ -e "$grammar_overlay" || -L "$grammar_overlay" ]] ||
  /bin/ln -s "$grammar_source" "$grammar_overlay"
[[ -e "$xandeer_overlay" || -L "$xandeer_overlay" ]] ||
  /bin/ln -s "$xandeer_source" "$xandeer_overlay"

[[ -L "$grammar_overlay" && -L "$xandeer_overlay" ]] || exit 1
[[ "$(/usr/bin/readlink "$grammar_overlay")" == "$grammar_source" ]] || exit 1
[[ "$(/usr/bin/readlink "$xandeer_overlay")" == "$xandeer_source" ]] || exit 1
[[ -s "$feature_worktree/rime/xandeer.dict.yaml" ]] || exit 1
git -C "$feature_worktree" check-ignore -q rime/amz-v2n3m1-zh-hans.gram
git -C "$feature_worktree" check-ignore -q etc/xandeer.dict.yaml
```

若目标已存在但不是上述精确 symlink，立即停止，不覆盖。overlay 必须保持 ignored，不能 stage 或 commit。

### Step 2: 在同一个 fail-fast shell 中安装、编译并检查

`native_root` 是当前 shell 的局部变量，所以安装、deployer 与日志检查必须在同一个 block 中执行，不能拆到新的 `exec_command`：

```zsh
set -eu
native_root="$(mktemp -d /tmp/rime-auto-space-native.XXXXXX)"
make -C rime HOME="$native_root" install
mkdir -p "$native_root/Library/Rime/build"

GLOG_logtostderr=1 \
  '/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_deployer' \
  --build \
  "$native_root/Library/Rime" \
  '/Library/Input Methods/Squirrel.app/Contents/SharedSupport' \
  "$native_root/Library/Rime/build" \
  >"$native_root/deploy.log" 2>&1

rg --files "$native_root/Library/Rime/build" | rg 'double_pinyin_flypy'

deploy_error_status=0
rg -n '(?i)(lua.*(error|failed)|encode failure|component.*(error|failed)|fatal)' \
  "$native_root/deploy.log" || deploy_error_status=$?
case "$deploy_error_status" in
  0)
    print -u2 "native Rime deployment logged a target error"
    exit 1
    ;;
  1) ;;
  *)
    print -u2 "failed to inspect native deploy log (exit $deploy_error_status)"
    exit 1
    ;;
esac

print -- "native_root=$native_root"
```

Expected:

- build 中存在 `double_pinyin_flypy` 相关产物；
- 日志检查将 `rg` 的 0/1/>1 分开处理，只有 exit 1 表示没有目标错误模式；
- 若 deployer 本身非 0，则即使日志没有匹配也视为失败。

这一步只证明已安装的 Squirrel deployer 能编译 schema 和数据。Lua 语法/逻辑由 bundled-Lua harness 证明；这里不证明 filter 被真实 session 执行，也不证明 commit history、Shadow 解包、用户词典学习或宿主应用光标语义。

保留最后打印的精确 `native_root` 路径到本任务结束，便于检查日志；不要用 broad glob 重新解析它，也不要把它替换成当前用户 HOME。

### Step 3: 最终自动化证据

在即将声称实现完成前使用 `superpowers:verification-before-completion`，重新运行：

```zsh
set -eu
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
feature_base="$(git merge-base master HEAD)"
git diff --check "$feature_base"..HEAD
git diff --name-status "$feature_base"..HEAD
git status --short --branch
git log --oneline "$feature_base"..HEAD
```

记录退出码与两个 `OK` 行。不要因为临时 deploy 成功而宣称 Task 7 的真实输入验收已完成。

## Task 7: 经用户授权后的当前 Squirrel 真实验收

**Files:**

- Deploy: `/Users/kevin/.cache/rime`
- Deploy: `/Users/kevin/Library/Rime`
- Verify: `/Users/kevin/Library/Rime/ai_weights.tsv`
- Verify: 临时用户词典导出文件

### Step 1: 停止并请求授权

向用户明确说明以下动作会更新 `/Users/kevin/.cache/rime` 和 `/Users/kevin/Library/Rime`、要求 Squirrel 重新部署、数次完全退出并重启输入法进程，并在验收选词时永久改变真实用户词典与 `ai_weights.tsv`。没有覆盖这两处安装目标、进程中断和学习数据变更的明确授权不得继续。

### Step 2: 授权后安装并重新部署

先重新执行 Task 6 Step 1 的 ignored-dependency preflight，确认 overlay 仍准确且未被 stage。再从仍然存在的 feature worktree 执行：

```zsh
set -eu
make -C rime install
'/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_deployer' \
  --build \
  /Users/kevin/Library/Rime \
  '/Library/Input Methods/Squirrel.app/Contents/SharedSupport' \
  /Users/kevin/Library/Rime/build
```

然后通过 Squirrel 菜单重新部署/重载。不要在验收完成前清理 worktree，因为 Makefile 会把 active grammar symlink 指向当前 checkout。

### Step 3: 执行真实输入矩阵

在普通文本输入框中逐项验证：

```text
一个 -> 候选 harness -> 候选来 = 一个 harness 来
候选 Rime -> 候选输入法       = Rime 输入法
直接键入 R -> 候选输入法    = R输入法
第 -> 3次                = 第3次
中文 -> 手动空格 -> Rime = 中文 Rime
中文，-> Rime            = 中文，Rime
中文🙂 -> Rime           = 中文🙂Rime
```

另测：

- `Ctrl+J/L` 的单 segment 与已确认首 segment + 活动第二 segment；
- `Ctrl+J/L` 上屏后的下一候选不自动补空格，因为 `engine:commit_text()` 产生 `raw` 记录；
- 回车、退格后的空历史；
- 简繁切换候选的 display/comment；
- 普通字典、melt_eng、自定义短语、`ai`、`ai_learned` 候选；
- 候选顺序无肉眼可见退化。

鼠标移动光标、粘贴、切换应用和宿主应用自行修改文本不在保证范围；如实记录，不把它们算作回归失败或已覆盖功能。

### Step 4: 检查用户词典与 AI 学习

先解析实际用户词典名：

```zsh
(
  set -eu
  cd /Users/kevin/Library/Rime
  '/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_dict_manager' --list
)
```

`rime_dict_manager --export` 需要独占 LevelDB lock；仅 reload 不足够。before snapshot 的严格生命周期是：

1. 先在输入法菜单切换到 ABC 等非 Squirrel 输入源，避免有焦点的文本客户端自动拉起 Squirrel；
2. 完全退出 Squirrel，并有界轮询确认进程不存在；
3. 按实际词典名称导出到 `/tmp`，任何 export 非零都立即停止；
4. 保留并记录导出路径，后续不能用 glob 猜测。

退出与 before export 使用 fail-fast 命令；把占位值替换为 `--list` 返回的精确名称：

```zsh
set -eu
# 用户必须先手动切换到非 Squirrel 输入源。
if /usr/bin/pgrep -x Squirrel >/dev/null; then
  /usr/bin/killall Squirrel
fi
for shutdown_attempt in {1..20}; do
  if ! /usr/bin/pgrep -x Squirrel >/dev/null; then
    break
  fi
  /bin/sleep 0.25
done
if /usr/bin/pgrep -x Squirrel >/dev/null; then
  print -u2 "Squirrel still holds the user dictionary"
  exit 1
fi

dictionary_name='<resolved-user-dictionary-name>'
before_export="$(mktemp /tmp/rime-auto-space-userdb-before.XXXXXX)"
(
  cd /Users/kevin/Library/Rime
  '/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_dict_manager' \
    --export "$dictionary_name" "$before_export"
)
print -- "before_export=$before_export"
```

把打印的精确 before 路径记录到验收日志；禁止稍后用 glob 猜测。随后严格按此顺序执行，不能把 after export 提前：

1. 从当前 `ai_weights.tsv` 记录将要选择的已知 `ai_learned` 精确 row、weight 和 mode；
2. 重新选择 Squirrel 输入法，用 `/usr/bin/pgrep -x Squirrel` 确认进程已启动；
3. 在 ASCII ↔ Han 边界下选择一个已记录文本/编码的普通词典候选恰好一次；
4. 在另一个边界下选择已记录的 `ai_learned` 候选恰好一次，并记录带空格上屏文本；
5. 开始并取消一个新的 composition，给普通用户词典 Memory 刷新机会；
6. 手动切换到非 Squirrel 输入源；
7. 终止 Squirrel，并重复有界轮询直到进程不存在；
8. 创建全新的 after 路径、导出用户词典、打印并记录该精确路径；
9. 读取 after `ai_weights.tsv` 精确 row 与 mode；
10. 完成比较后，再由用户决定是否重新选择 Squirrel。

Step 7–8 使用独立的 fail-fast block；`dictionary_name` 必须与 before 相同：

```zsh
set -eu
# 用户必须先手动切换到非 Squirrel 输入源。
if /usr/bin/pgrep -x Squirrel >/dev/null; then
  /usr/bin/killall Squirrel
fi
for shutdown_attempt in {1..20}; do
  if ! /usr/bin/pgrep -x Squirrel >/dev/null; then
    break
  fi
  /bin/sleep 0.25
done
if /usr/bin/pgrep -x Squirrel >/dev/null; then
  print -u2 "Squirrel still holds the user dictionary"
  exit 1
fi

dictionary_name='<same-resolved-user-dictionary-name>'
after_export="$(mktemp /tmp/rime-auto-space-userdb-after.XXXXXX)"
(
  cd /Users/kevin/Library/Rime
  '/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_dict_manager' \
    --export "$dictionary_name" "$after_export"
)
print -- "after_export=$after_export"
```

把打印的 before/after 路径和 AI 精确 row 快照一起记录。比较同一普通词典词条：

- 无前导空格的 row 从不存在变为存在，或其 count/weight 相对 before snapshot 发生学习性变化；
- 对应的 `" <text>"` row 在 before/after 中都不存在；
- 记录选中的 candidate 文本、编码和 before/after 字段，避免把历史已有 row 误当成本次学习证据。

对 `ai_weights.tsv` 的同一精确 row 断言：

- 上屏为带排版空格的 display；
- 无前导空格 genuine row 的 weight 恰好 `+1`；
- 不存在对应的带前导空格 display row；
- 文件权限仍为 `0600`。

live `ai` bridge 作为单独人工场景验证，不把它与本地 `ai_learned` 的确定性 TSV 断言混为一谈。

public Rime API 不暴露 raw quality，因此只报告顺序、comment 与学习结果，不宣称运行时 quality 数值完全相等。

### Step 5: 完成分支

真实验收通过后使用 `superpowers:finishing-a-development-branch`，向用户提供 merge、PR、保留分支或清理选项。merge、PR 或保留分支可单独选择；任何会删除 feature worktree 的选项都必须先再次取得安装授权，从最终保留的 checkout 重新执行 Rime install，并确认 active grammar symlink 不再指向 feature worktree。若未获该授权，允许整合分支，但必须保留 worktree。除非用户明确选择，不 merge、不 push、不删除 worktree。
