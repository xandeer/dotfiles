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

local function same_candidate_span(final, genuine)
    local final_start = tonumber(final.start)
    local final_end = tonumber(final._end)
    local genuine_start = tonumber(genuine.start)
    local genuine_end = tonumber(genuine._end)
    return final_start ~= nil and final_end ~= nil and
        genuine_start ~= nil and genuine_end ~= nil and
        final_start == genuine_start and final_end == genuine_end
end

local function is_candidate_value(value)
    local value_type = type(value)
    return value_type == "table" or value_type == "userdata"
end

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
    local output_provenance = {}

    local function yield_other(candidate, text)
        if output_provenance[text] == nil then
            output_provenance[text] = "other"
        end
        yield(candidate)
    end

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
                local spaced = " " .. text
                local provenance = output_provenance[spaced]
                if provenance == "other" then
                    yield_other(candidate, text)
                elseif provenance == nil then
                    local seed_ok, seed = pcall(
                        UniquifiedCandidate,
                        genuine,
                        "auto_space",
                        spaced,
                        display_comment
                    )
                    if seed_ok and is_candidate_value(seed) then
                        output_provenance[spaced] = "auto"
                        yield(seed)
                    else
                        output_provenance[spaced] = "other"
                        yield_other(candidate, text)
                    end
                else
                    local wrap_ok, wrapped = pcall(
                        ShadowCandidate,
                        genuine,
                        "auto_space",
                        spaced,
                        display_comment
                    )
                    if wrap_ok and is_candidate_value(wrapped) then
                        yield(wrapped)
                    else
                        yield_other(candidate, text)
                    end
                end
            else
                yield_other(candidate, text)
            end
        else
            yield_other(candidate, text)
        end
    end
end

local function auto_space_prefix(context, commit_text)
    local proof_ok, prefix = pcall(function()
        if type(commit_text) ~= "string" then
            return ""
        end

        local composition = context.composition
        local composition_type = type(composition)
        if composition_type ~= "table" and composition_type ~= "userdata" then
            return ""
        end
        if composition:empty() ~= false then
            return ""
        end

        local segmentation = composition:toSegmentation()
        local segmentation_type = type(segmentation)
        if segmentation_type ~= "table" and segmentation_type ~= "userdata" then
            return ""
        end
        if segmentation:empty() ~= false then
            return ""
        end

        local first_segment = segmentation:get_at(0)
        local segment_type = type(first_segment)
        if segment_type ~= "table" and segment_type ~= "userdata" then
            return ""
        end

        local selected = first_segment:get_selected_candidate()
        local selected_type = type(selected)
        if selected_type ~= "table" and selected_type ~= "userdata" then
            return ""
        end

        local candidate_type = selected.type
        local display = selected.text
        if candidate_type ~= "auto_space" or type(display) ~= "string" or
            display:sub(1, 1) ~= " " or display:sub(2, 2) == " " or
            commit_text:sub(1, #display) ~= display then
            return ""
        end

        local genuine = selected:get_genuine()
        local genuine_type = type(genuine)
        if genuine_type ~= "table" and genuine_type ~= "userdata" then
            return ""
        end

        return " "
    end)

    return proof_ok and prefix or ""
end

function select_character(key, env)
    local engine = env.engine
    local context = engine.context
    local config = engine.schema.config

    -- local first_key = config:get_string('key_binder/select_first_character') or 'bracketleft'
    -- local last_key = config:get_string('key_binder/select_last_character') or 'bracketright'
    local first_key = config:get_string('key_binder/select_first_character')
    local last_key = config:get_string('key_binder/select_last_character')
    local key_repr = key:repr()
    if key_repr ~= first_key and key_repr ~= last_key then
        return 2 -- kNoop
    end

    local commit_text = context:get_commit_text()
    local prefix = auto_space_prefix(context, commit_text)
    local selectable_text = prefix == "" and commit_text or commit_text:sub(2)

    if (key_repr == first_key and selectable_text ~= "") then
        engine:commit_text(prefix .. first_character(selectable_text))
        context:clear()

        return 1 -- kAccepted
    end

    if (key_repr == last_key and selectable_text ~= "") then
        engine:commit_text(prefix .. last_character(selectable_text))
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
    local directory = rime_api.get_user_data_dir()
    if type(directory) ~= "string" then
        return nil
    end
    return directory .. "/ai_weights.tsv"
end

local function ai_valid_path(path)
    return type(path) == "string" and path:sub(1, 1) == "/" and
        not path:find("\0", 1, true) and not path:find("\r", 1, true) and
        not path:find("\n", 1, true)
end

local function ai_shell_quote(value)
    return "'" .. value:gsub("'", "'\"'\"'") .. "'"
end

local function ai_command_succeeded(result)
    return result == true or result == 0
end

local function ai_read_pipe(pipe, format)
    -- Squirrel signals can interrupt a pipe read; retry only macOS EINTR (4).
    for _ = 1, 32 do
        local value, _, error_code = pipe:read(format)
        if value ~= nil or error_code ~= 4 then
            return value
        end
    end
    return nil
end

local function ai_storage_kind(path)
    -- macOS stat uses lstat semantics by default, so symbolic links stay visible.
    local pipe = io.popen("/usr/bin/stat -f %p " .. ai_shell_quote(path) .. " 2>/dev/null", "r")
    if pipe then
        local mode = ai_read_pipe(pipe, "*l")
        local extra = ai_read_pipe(pipe, "*a")
        local close_result = pipe:close()
        if extra == "" and ai_command_succeeded(close_result) then
            if type(mode) == "string" and mode:match("^10[0-7][0-7][0-7][0-7]$") then
                return "regular"
            end
            return "special"
        end
    end
    -- A same-path rename distinguishes a missing file without opening FIFOs/devices.
    return os.rename(path, path) and "special" or "missing"
end

local function ai_prepare_storage(path)
    if not ai_valid_path(path) then
        return false
    end

    local kind = ai_storage_kind(path)
    if kind == "missing" then
        return true
    end
    if kind ~= "regular" then
        return false
    end
    return ai_command_succeeded(os.execute("/bin/chmod 600 " .. ai_shell_quote(path)))
end

local function ai_ensure_storage(env)
    if env.ai_storage_ready == nil then
        env.ai_weights_path = ai_weights_path()
        env.ai_storage_ready = ai_prepare_storage(env.ai_weights_path)
    end
    return env.ai_storage_ready
end

local function ai_valid_field(value)
    return type(value) == "string" and value ~= "" and not value:find("[\t\r\n]")
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
        if ai_valid_field(schema_id) and ai_valid_field(input) and ai_valid_field(text) and
            weight and last_used and
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

local function ai_create_temporary(path)
    local prefix = path .. ".tmp."
    local pipe = io.popen("/usr/bin/mktemp -q " .. ai_shell_quote(prefix .. "XXXXXX"), "r")
    if not pipe then
        return nil
    end

    local temporary = ai_read_pipe(pipe, "*l")
    local extra = ai_read_pipe(pipe, "*a")
    local close_result = pipe:close()
    if temporary == nil and type(extra) == "string" then
        temporary = extra:match("^([^\r\n]+)\n?$")
        if temporary then
            extra = ""
        end
    end
    local suffix = type(temporary) == "string" and temporary:sub(#prefix + 1) or ""
    local valid = type(temporary) == "string" and temporary:sub(1, #prefix) == prefix and
        suffix:match("^[A-Za-z0-9_-]+$") ~= nil
    if not valid then
        return nil
    end
    if extra ~= "" or not ai_command_succeeded(close_result) then
        os.remove(temporary)
        return nil
    end
    return temporary
end

local function ai_write_learning(path, pending)
    if not ai_valid_path(path) then
        return false
    end
    if not ai_valid_field(pending.schema_id) or not ai_valid_field(pending.input) or
        not ai_valid_field(pending.text) then
        return false
    end

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

    local temporary = ai_create_temporary(path)
    if not temporary then
        return false
    end
    local file = io.open(temporary, "r+")
    if not file then
        os.remove(temporary)
        return false
    end
    local end_position = file:seek("end")
    local start_position = end_position == 0 and file:seek("set", 0) or nil
    if end_position ~= 0 or start_position ~= 0 then
        file:close()
        os.remove(temporary)
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
    ai_ensure_storage(env)
    env.ai_connections = {
        context.select_notifier:connect(function(current)
            if not env.ai_storage_ready then
                env.ai_pending = nil
                return
            end
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
            if pending and env.ai_storage_ready and
                not ai_write_learning(env.ai_weights_path, pending) then
                env.ai_storage_ready = false
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
    if not ai_ensure_storage(env) then
        return
    end
    local rows, read_ok = ai_read_rows(env.ai_weights_path)
    if not read_ok then
        env.ai_storage_ready = false
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
    env.ai_weights_path = nil
    env.ai_storage_ready = nil
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
