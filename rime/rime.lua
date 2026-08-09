-- Rime Lua 扩展 https://github.com/hchunhui/librime-lua
-- 文档 https://github.com/hchunhui/librime-lua/wiki/Scripting
-------------------------------------------------------------
-- 日期时间
-- 提高权重的原因：因为在方案中设置了大于 1 的 initial_quality，导致 rq sj xq dt ts 产出的候选项在所有词语的最后。
function date_translator(input, seg, env)
    local config = env.engine.schema.config
    local date = config:get_string(env.name_space .. "/date") or "rq"
    local time = config:get_string(env.name_space .. "/time") or "sj"
    local week = config:get_string(env.name_space .. "/week") or "xq"
    local datetime = config:get_string(env.name_space .. "/datetime") or "dt"
    local timestamp = config:get_string(env.name_space .. "/timestamp") or "ts"
    -- 日期
    if (input == date) then
        local cand = Candidate("date", seg.start, seg._end, os.date("%Y-%m-%d"), "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("date", seg.start, seg._end, os.date("%Y/%m/%d"), "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("date", seg.start, seg._end, os.date("%Y.%m.%d"), "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("date", seg.start, seg._end, os.date("%Y 年 %m 月 %d 日"), "")
        cand.quality = 100
        yield(cand)
    end
    -- 时间
    if (input == time) then
        local cand = Candidate("time", seg.start, seg._end, os.date("%H:%M"), "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("time", seg.start, seg._end, os.date("%H:%M:%S"), "")
        cand.quality = 100
        yield(cand)
    end
    -- 星期
    if (input == week) then
        local weakTab = {'日', '一', '二', '三', '四', '五', '六'}
        local cand = Candidate("week", seg.start, seg._end, "星期" .. weakTab[tonumber(os.date("%w") + 1)], "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("week", seg.start, seg._end, "礼拜" .. weakTab[tonumber(os.date("%w") + 1)], "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("week", seg.start, seg._end, "周" .. weakTab[tonumber(os.date("%w") + 1)], "")
        cand.quality = 100
        yield(cand)
    end
    -- ISO 8601/RFC 3339 的时间格式 （固定东八区）（示例 2022-01-07T20:42:51+08:00）
    if (input == datetime) then
        local cand = Candidate("datetime", seg.start, seg._end, os.date("%Y-%m-%dT%H:%M:%S+08:00"), "")
        cand.quality = 100
        yield(cand)
        local cand = Candidate("time", seg.start, seg._end, os.date("%Y%m%d%H%M%S"), "")
        cand.quality = 100
        yield(cand)
    end
    -- 时间戳（十位数，到秒，示例 1650861664）
    if (input == timestamp) then
        local cand = Candidate("datetime", seg.start, seg._end, os.time(), "")
        cand.quality = 100
        yield(cand)
    end
end
-------------------------------------------------------------
-- 以词定字
-- https://github.com/BlindingDark/rime-lua-select-character
-- 删除了默认按键，需要在 key_binder 下设置
local function utf8_sub(s, i, j)
    i = i or 1
    j = j or -1

    if i < 1 or j < 1 then
        local n = utf8.len(s)
        if not n then
            return nil
        end
        if i < 0 then
            i = n + 1 + i
        end
        if j < 0 then
            j = n + 1 + j
        end
        if i < 0 then
            i = 1
        elseif i > n then
            i = n
        end
        if j < 0 then
            j = 1
        elseif j > n then
            j = n
        end
    end

    if j < i then
        return ""
    end

    i = utf8.offset(s, i)
    j = utf8.offset(s, j + 1)

    if i and j then
        return s:sub(i, j - 1)
    elseif i then
        return s:sub(i)
    else
        return ""
    end
end

local function first_character(s)
    return utf8_sub(s, 1, 1)
end

local function last_character(s)
    return utf8_sub(s, -1, -1)
end

function select_character(key, env)
    local engine = env.engine
    local context = engine.context
    local commit_text = context:get_commit_text()
    local config = engine.schema.config

    -- local first_key = config:get_string('key_binder/select_first_character') or 'bracketleft'
    -- local last_key = config:get_string('key_binder/select_last_character') or 'bracketright'
    local first_key = config:get_string('key_binder/select_first_character')
    local last_key = config:get_string('key_binder/select_last_character')

    if (key:repr() == first_key and commit_text ~= "") then
        engine:commit_text(first_character(commit_text))
        context:clear()

        return 1 -- kAccepted
    end

    if (key:repr() == last_key and commit_text ~= "") then
        engine:commit_text(last_character(commit_text))
        context:clear()

        return 1 -- kAccepted
    end

    return 2 -- kNoop
end
-------------------------------------------------------------
-- 长词优先（提升「西安」「提案」「图案」「饥饿」等词汇的优先级）
-- 感谢&参考于： https://github.com/tumuyan/rime-melt
-- 修改：不提升英文和中英混输的
function long_word_filter(input, env)
    -- 提升 count 个词语，插入到第 idx 个位置，默认 2、4。
    local config = env.engine.schema.config
    local count = config:get_int(env.name_space .. "/count") or 2
    local idx = config:get_int(env.name_space .. "/idx") or 4

    local l = {}
    local firstWordLength = 0 -- 记录第一个候选词的长度，提前的候选词至少要比第一个候选词长
    local s = 0 -- 记录筛选了多少个词条(只提升 count 个词的权重)

    local i = 1
    for cand in input:iter() do
        local leng = utf8.len(cand.text)
        if (firstWordLength < 1 or i < idx) then
            i = i + 1
            firstWordLength = leng
            yield(cand)
        elseif ((leng > firstWordLength) and (s < count)) and (string.find(cand.text, "[%w%p%s]+") == nil) then
            yield(cand)
            s = s + 1
        else
            table.insert(l, cand)
        end
    end
    for _, cand in ipairs(l) do
        yield(cand)
    end
end
-------------------------------------------------------------
-- 降低部分英语单词在候选项的位置
-- https://dvel.me/posts/make-rime-en-better/#短单词置顶的问题
-- 感谢大佬 @[Shewer Lu](https://github.com/shewer) 指点
function reduce_english_filter(input, env)
    local config = env.engine.schema.config
    -- load data
    if not env.idx then
        env.idx = config:get_int(env.name_space .. "/idx") -- 要插入的位置
    end
    if not env.words then
        env.words = {} -- 要过滤的词
        local list = config:get_list(env.name_space .. "/words")
        for i = 0, list.size - 1 do
            local word = list:get_value_at(i).value
            env.words[word] = true
        end
    end

    -- filter start
    local code = env.engine.context.input
    if env.words[code] then
        local pending_cands = {}
        local index = 0
        for cand in input:iter() do
            index = index + 1
            if string.lower(cand.text) == code then
                table.insert(pending_cands, cand)
            else
                yield(cand)
            end
            if index >= env.idx + #pending_cands - 1 then
                for _, cand in ipairs(pending_cands) do
                    yield(cand)
                end
                break
            end
        end
    end

    -- yield other
    for cand in input:iter() do
        yield(cand)
    end
end
-------------------------------------------------------------
-- v 模式，单个字符优先
-- 因为设置了英文翻译器的 initial_quality 大于 1，导致输入「va」时，候选项是「van vain …… ā á ǎ à」
-- 把候选项应改为「ā á ǎ à …… van vain」，让单个字符的排在前面
-- 感谢改进 @[t123yh](https://github.com/t123yh) @[Shewer Lu](https://github.com/shewer)
function v_filter(input, env)
    local code = env.engine.context.input -- 当前编码
    env.v_spec_arr = env.v_spec_arr or Set(
        {"0️⃣", "1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣", "6️⃣", "7️⃣", "8️⃣", "9️⃣", "Vs."})
    -- 仅当当前输入以 v 开头，并且编码长度为 2，才进行处理
    if (string.len(code) == 2 and string.find(code, "^v")) then
        local l = {}
        for cand in input:iter() do
            -- 特殊情况处理
            if (env.v_spec_arr[cand.text]) then
                yield(cand)
                -- 候选项为单个字符的，提到前面来。
            elseif (utf8.len(cand.text) == 1) then
                yield(cand)
            else
                table.insert(l, cand)
            end
        end
        for _, cand in ipairs(l) do
            yield(cand)
        end
    else
        for cand in input:iter() do
            yield(cand)
        end
    end
end
-------------------------------------------------------------
-- iRime 九宫格专用，将输入框的数字转为对应的拼音或英文
function irime_t9_preedit(input, env)
    for cand in input:iter() do
        if (string.find(cand.text, "%w+") ~= nil) then
            cand:get_genuine().preedit = cand.text
        else
            cand:get_genuine().preedit = cand.comment
        end
        yield(cand)
    end
end
-------------------------------------------------------------
-- Unicode 输入
-- 复制自： https://github.com/shewer/librime-lua-script/blob/main/lua/component/unicode.lua
function unicode(input, seg, env)
    local ucodestr = seg:has_tag("unicode") and input:match("U(%x+)")
    if ucodestr and #ucodestr > 1 then
        local code = tonumber(ucodestr, 16)
        local text = utf8.char(code)
        yield(Candidate("unicode", seg.start, seg._end, text, string.format("U%x", code)))
        if #ucodestr < 5 then
            for i = 0, 15 do
                local text = utf8.char(code * 16 + i)
                yield(Candidate("unicode", seg.start, seg._end, text, string.format("U%x~%x", code, i)))
            end
        end
    end
end
-------------------------------------------------------------
-- AI candidate ordering and the small learned correction lexicon.
local function ai_weights_path()
    return rime_api.get_user_data_dir() .. "/ai_weights.tsv"
end

local function ai_valid_field(value)
    return type(value) == "string" and not value:find("[\t\r\n]")
end

local function ai_read_rows(path)
    -- ponytail: whole-file TSV is intentional for a small personal lexicon; use a DB only after measured growth makes this slow.
    local file = io.open(path, "r")
    if not file then
        if os.rename(path, path) then
            return nil, false
        end
        return {}, true
    end
    local contents = file:read("*a")
    local closed = file:close()
    if contents == nil or not closed then
        return nil, false
    end

    local rows = {}
    for line in (contents .. "\n"):gmatch("(.-)\n") do
        local schema_id, input, text, raw_weight, raw_time =
            line:match("^([^\t\r\n]*)\t([^\t\r\n]*)\t([^\t\r\n]*)\t([^\t\r\n]*)\t([^\t\r\n]*)$")
        local weight, last_used = tonumber(raw_weight), tonumber(raw_time)
        if schema_id and weight and last_used and
            weight >= 0 and weight < math.huge and last_used >= 0 and last_used < math.huge then
            rows[#rows + 1] = {
                schema_id = schema_id,
                input = input,
                text = text,
                weight = weight,
                last_used = last_used,
            }
        end
    end
    return rows, true
end

local function ai_row_key(row)
    return row.schema_id .. "\0" .. row.input .. "\0" .. row.text
end

local function ai_write_learning(pending)
    if not ai_valid_field(pending.schema_id) or not ai_valid_field(pending.input) or
        not ai_valid_field(pending.text) or pending.schema_id == "" or
        pending.input == "" or pending.text == "" then
        return false
    end

    local path = ai_weights_path()
    local rows, read_ok = ai_read_rows(path)
    if not read_ok then
        return false
    end

    local merged = {}
    for _, row in ipairs(rows) do
        local key = ai_row_key(row)
        local previous = merged[key]
        if not previous or row.weight > previous.weight or
            (row.weight == previous.weight and row.last_used > previous.last_used) then
            merged[key] = row
        end
    end

    local key = ai_row_key(pending)
    local row = merged[key] or {
        schema_id = pending.schema_id,
        input = pending.input,
        text = pending.text,
        weight = 0,
        last_used = 0,
    }
    row.weight = math.min(row.weight + 1, 1000000)
    row.last_used = os.time()
    merged[key] = row

    rows = {}
    for _, value in pairs(merged) do
        rows[#rows + 1] = value
    end
    table.sort(rows, function(left, right)
        if left.schema_id ~= right.schema_id then
            return left.schema_id < right.schema_id
        end
        if left.input ~= right.input then
            return left.input < right.input
        end
        return left.text < right.text
    end)

    local temporary = path .. ".tmp"
    local file = io.open(temporary, "w")
    if not file then
        return false
    end
    local write_ok = true
    for _, value in ipairs(rows) do
        if not file:write(table.concat({
            value.schema_id,
            value.input,
            value.text,
            tostring(value.weight),
            tostring(value.last_used),
        }, "\t"), "\n") then
            write_ok = false
            break
        end
    end
    local close_ok = file:close()
    if not write_ok or not close_ok then
        os.remove(temporary)
        return false
    end
    local rename_ok = os.rename(temporary, path)
    if not rename_ok then
        os.remove(temporary)
        return false
    end
    return true
end

local function ai_segment_is_selected(segment)
    local status = tostring(segment.status):lower()
    return status == "2" or status == "3" or
        status:find("selected", 1, true) ~= nil or
        status:find("confirmed", 1, true) ~= nil
end

ai_learned_translator = {}

function ai_learned_translator.init(env)
    local context = env.engine.context
    env.ai_connections = {
        context.select_notifier:connect(function(current)
            local segment = current.composition:empty() and nil or current.composition:back()
            local candidate = current:get_selected_candidate()
            local genuine = candidate and candidate:get_genuine() or nil
            if not segment or not genuine then
                env.ai_pending = nil
                return
            end

            local candidate_type = tostring(genuine.type or "")
            local generation = current:get_property("_ai_generation") or ""
            if generation == "" and candidate_type ~= "ai" and candidate_type ~= "ai_learned" then
                env.ai_pending = nil
                return
            end

            local full_input = tostring(current.input or "")
            local start_pos, end_pos = tonumber(segment.start), tonumber(segment._end)
            local schema_id = tostring(env.engine.schema.schema_id or "")
            local text = tostring(genuine.text or "")
            if not start_pos or not end_pos or start_pos < 0 or end_pos < start_pos then
                env.ai_pending = nil
                return
            end
            env.ai_pending = {
                full_input = full_input,
                start = start_pos,
                _end = end_pos,
                schema_id = schema_id,
                input = full_input:sub(start_pos + 1, end_pos),
                type = candidate_type,
                text = text,
            }
        end, 0),
        context.commit_notifier:connect(function()
            local pending = env.ai_pending
            env.ai_pending = nil
            if pending then
                ai_write_learning(pending)
            end
        end, 0),
        context.update_notifier:connect(function(current)
            local pending = env.ai_pending
            if not pending then
                return
            end
            if current.composition:empty() or tostring(current.input or "") ~= pending.full_input then
                env.ai_pending = nil
                return
            end
            local segment = current.composition:back()
            if segment and tonumber(segment.start) == pending.start and
                tonumber(segment._end) == pending._end and not ai_segment_is_selected(segment) then
                env.ai_pending = nil
            end
        end, 0),
    }
end

function ai_learned_translator.func(input, seg, env)
    local rows = ai_read_rows(ai_weights_path())
    if not rows then
        return
    end
    local schema_id = tostring(env.engine.schema.schema_id or "")
    local baseline = tonumber(env.engine.schema.config:get_double("translator/initial_quality")) or 1
    local matching = {}
    for _, row in ipairs(rows) do
        if row.schema_id == schema_id and row.input == input then
            matching[#matching + 1] = row
        end
    end
    table.sort(matching, function(left, right)
        if left.weight ~= right.weight then
            return left.weight > right.weight
        end
        if left.last_used ~= right.last_used then
            return left.last_used > right.last_used
        end
        return left.text < right.text
    end)
    for _, row in ipairs(matching) do
        local candidate = Candidate("ai_learned", seg.start, seg._end, row.text, "AI")
        candidate.quality = baseline + math.min(row.weight, 100) / 1000
        yield(candidate)
    end
end

function ai_learned_translator.fini(env)
    for _, connection in ipairs(env.ai_connections or {}) do
        connection:disconnect()
    end
    env.ai_connections = nil
    env.ai_pending = nil
end

function ai_candidate_filter(input, env)
    local context = env.engine.context
    local text = context:get_property("_ai_candidate") or ""
    local ai_input = context:get_property("_ai_input") or ""
    local generation = context:get_property("_ai_generation") or ""
    local segment = context.composition:empty() and nil or context.composition:back()
    if text == "" or ai_input ~= context.input or generation == "" or not segment then
        for candidate in input:iter() do
            yield(candidate)
        end
        return
    end

    local iterator, state = input:iter()
    local buffered, match_index = {}, nil
    -- ponytail: scan only the candidates sent to AI; raise this bound only if unseen deep duplicates are measured.
    for index = 1, 8 do
        local candidate = iterator(state)
        if not candidate then
            break
        end
        buffered[#buffered + 1] = candidate
        if not match_index and candidate.text == text then
            match_index = index
        end
    end

    if match_index then
        yield(buffered[match_index])
    else
        yield(Candidate("ai", segment.start, segment._end, text, "AI"))
    end
    for index, candidate in ipairs(buffered) do
        if index ~= match_index then
            yield(candidate)
        end
    end
    for candidate in iterator, state do
        yield(candidate)
    end
end
-------------------------------------------------------------
