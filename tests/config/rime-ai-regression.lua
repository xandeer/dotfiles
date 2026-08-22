assert(type(ai_candidate_filter) == "function", "missing production ai_candidate_filter")
assert(type(auto_space_filter) == "function", "missing production auto_space_filter")
assert(type(ai_learned_translator) == "table", "missing production ai_learned_translator")
assert(type(ai_learned_translator.init) == "function", "missing ai_learned_translator.init")
assert(type(ai_learned_translator.func) == "function", "missing ai_learned_translator.func")
assert(type(ai_learned_translator.fini) == "function", "missing ai_learned_translator.fini")

local function same(actual, expected, message)
    assert(actual == expected, (message or "values differ") ..
        ": expected " .. tostring(expected) .. ", got " .. tostring(actual))
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

local function is_candidate(value)
    local value_type = type(value)
    return value_type == "table" or value_type == "userdata"
end

local function unpack_shadow_once(value)
    if is_candidate(value) and value:get_dynamic_type() == "Shadow" then
        return value:get_genuine()
    end
    return value
end

local function uniquified(kind, text, comment, source)
    local value = {
        type = kind,
        start = source.start,
        _end = source._end,
        text = text ~= "" and text or source.text,
        comment = comment ~= "" and comment or source.comment,
        quality = source.quality,
        items = {source},
    }
    function value:get_dynamic_type()
        return "Uniquified"
    end
    function value:append(item)
        self.items[#self.items + 1] = item
        if self.quality < item.quality then
            self.quality = item.quality
        end
        return true
    end
    function value:get_genuine()
        return unpack_shadow_once(self.items[1])
    end
    function value:get_genuines()
        local genuines = {}
        for index, item in ipairs(self.items) do
            genuines[index] = unpack_shadow_once(item)
        end
        return genuines
    end
    return value
end

local shadow_calls = {}
local shadow_mode = "normal"
ShadowCandidate = function(source, kind, text, comment)
    shadow_calls[#shadow_calls + 1] = {
        source = source,
        kind = kind,
        text = text,
        comment = comment,
    }
    if shadow_mode == "throw" then
        error("injected ShadowCandidate failure")
    elseif shadow_mode == "nil" then
        return nil
    elseif shadow_mode == "noncandidate" then
        return "not a candidate"
    end
    return wrapper("Shadow", kind, text, comment, source)
end

local uniquified_calls = {}
local uniquified_mode = "normal"
UniquifiedCandidate = function(source, kind, text, comment)
    uniquified_calls[#uniquified_calls + 1] = {
        source = source,
        kind = kind,
        text = text,
        comment = comment,
    }
    if uniquified_mode == "throw" then
        error("injected UniquifiedCandidate failure")
    elseif uniquified_mode == "nil" then
        return nil
    elseif uniquified_mode == "noncandidate" then
        return "not a candidate"
    end
    return uniquified(kind, text or "", comment or "", source)
end

Candidate = function(kind, start_pos, end_pos, text, comment)
    return candidate(kind, start_pos, end_pos, text, comment)
end

local yielded = nil
local incoming_reads = 0
local reads_before_first_yield = nil
yield = function(value)
    if reads_before_first_yield == nil then
        reads_before_first_yield = incoming_reads
    end
    yielded[#yielded + 1] = value
end

local function stream(values)
    local state = {index = 0, values = values}
    return {
        iter = function()
            return function(current)
                assert(current == state, "translation iterator state must be forwarded")
                current.index = current.index + 1
                local value = current.values[current.index]
                if value ~= nil then
                    incoming_reads = incoming_reads + 1
                end
                return value
            end, state
        end,
    }
end

local function run_auto_filter(history, values)
    local back_reads = 0
    local latest_text_reads = 0
    local commit_history = {}
    function commit_history:back()
        back_reads = back_reads + 1
        return {type = "phrase", text = history}
    end
    function commit_history:latest_text()
        latest_text_reads = latest_text_reads + 1
        error("latest_text must not be consulted")
    end
    local filter_env = {
        engine = {
            context = {
                commit_history = commit_history,
            },
        },
    }
    yielded = {}
    incoming_reads = 0
    reads_before_first_yield = nil
    shadow_calls = {}
    uniquified_calls = {}
    auto_space_filter(stream(values), filter_env)
    same(latest_text_reads, 0, "auto filter latest_text read count")
    same(back_reads, 1, "auto filter history back read count")
    return yielded
end

local function auto_filter_translation(history, values)
    local back_reads = 0
    local latest_text_reads = 0
    local history_record = history
    if type(history_record) ~= "table" then
        history_record = {type = "phrase", text = history}
    end
    local commit_history = {}
    function commit_history:back()
        back_reads = back_reads + 1
        return history_record
    end
    function commit_history:latest_text()
        latest_text_reads = latest_text_reads + 1
        error("latest_text must not be consulted")
    end
    local filter_env = {
        engine = {
            context = {
                commit_history = commit_history,
            },
        },
    }

    incoming_reads = 0
    reads_before_first_yield = nil
    shadow_calls = {}
    uniquified_calls = {}
    local coroutine_handle = coroutine.create(function()
        auto_space_filter(stream(values), filter_env)
    end)
    local iterator_state = {}
    local translation = {
        iter = function()
            return function(current)
                assert(current == iterator_state,
                    "AI auto-filter iterator state must be forwarded")
                if coroutine.status(coroutine_handle) == "dead" then
                    return nil
                end
                local previous_yield = yield
                yield = function(value)
                    return coroutine.yield(value)
                end
                local ok, value = coroutine.resume(coroutine_handle)
                yield = previous_yield
                assert(ok, "lazy AI auto_space_filter failed: " .. tostring(value))
                return value
            end, iterator_state
        end,
    }
    local function verify_history_reads()
        same(latest_text_reads, 0, "lazy AI filter latest_text read count")
        same(back_reads, 1, "lazy AI filter history back read count")
    end
    return translation, verify_history_reads
end

local function run_builtin_uniquifier(translation)
    local menu = {}
    local iterator, state = translation:iter()
    while true do
        local next_candidate = iterator(state)
        if next_candidate == nil then
            break
        end
        local previous_index = nil
        for index, previous in ipairs(menu) do
            if previous.text == next_candidate.text then
                previous_index = index
                break
            end
        end
        if previous_index == nil then
            menu[#menu + 1] = next_candidate
        else
            local previous = menu[previous_index]
            if previous:get_dynamic_type() == "Uniquified" then
                previous:append(next_candidate)
            else
                previous = uniquified("uniquified", "", "", previous)
                previous:append(next_candidate)
                menu[previous_index] = previous
            end
        end
    end
    return menu
end

local function run_spacing_pipeline(history, values)
    local translation, verify_history_reads = auto_filter_translation(history, values)
    local menu = run_builtin_uniquifier(translation)
    verify_history_reads()
    return menu
end

local function composition(segment)
    return {
        segment = segment,
        empty = function(self)
            return self.segment == nil
        end,
        back = function(self)
            return self.segment
        end,
    }
end

local function notifier()
    local value = {slots = {}}
    function value:connect(callback, group)
        local slot = {callback = callback, group = group, connected = true}
        self.slots[#self.slots + 1] = slot
        return {
            disconnect = function()
                slot.connected = false
            end,
        }
    end
    function value:emit(context, engine_callback)
        for _, slot in ipairs(self.slots) do
            if slot.connected and slot.group == 0 then
                slot.callback(context)
            end
        end
        if engine_callback then
            engine_callback()
        end
        for _, slot in ipairs(self.slots) do
            if slot.connected and slot.group ~= 0 then
                slot.callback(context)
            end
        end
    end
    return value
end

local function context(input, segment)
    local value = {
        input = input,
        properties = {},
        composition = composition(segment),
        select_notifier = notifier(),
        commit_notifier = notifier(),
        update_notifier = notifier(),
    }
    function value:get_property(name)
        return self.properties[name] or ""
    end
    function value:set_property(name, property)
        self.properties[name] = property
    end
    function value:get_selected_candidate()
        return self.selected_candidate
    end
    function value:is_composing()
        return not self.composition:empty()
    end
    return value
end

local function env(input, segment, schema_id, initial_quality, weights_path)
    local ctx = context(input, segment)
    local configured_quality = initial_quality
    if initial_quality == nil then
        configured_quality = 1.2
    elseif initial_quality == false then
        configured_quality = nil
    end
    return {
        engine = {
            context = ctx,
            schema = {
                schema_id = schema_id or "test_schema",
                config = {
                    get_double = function(_, key)
                        same(key, "translator/initial_quality", "initial quality config key")
                        return configured_quality
                    end,
                    get_string = function(_, key)
                        same(key, "ai_learned_translator/weights_path", "weights path config key")
                        return weights_path
                    end,
                },
            },
        },
        name_space = "ai_learned_translator",
    }, ctx
end

local no_segment = {}
local function run_filter(values, properties, input, segment, history_record)
    local active_segment = segment
    if segment == nil then
        active_segment = {start = 0, _end = 4, status = "selected"}
    elseif segment == no_segment then
        active_segment = nil
    end
    local filter_env, ctx = env(input or "code", active_segment)
    ctx.properties = properties or {}
    ctx.commit_history = {
        back = function()
            return history_record
        end,
        latest_text = function()
            error("AI candidate filter must not consult aggregated history")
        end,
    }
    yielded = {}
    incoming_reads = 0
    reads_before_first_yield = nil
    ai_candidate_filter(stream(values), filter_env)
    return yielded, reads_before_first_yield
end

local function assert_originals_once(output, originals, offset)
    offset = offset or 0
    for _, original in ipairs(originals) do
        local count = 0
        for _, value in ipairs(output) do
            if rawequal(value, original) then
                count = count + 1
            end
        end
        same(count, 1, "original candidate replay count")
    end
    same(#output, #originals + offset, "output candidate count")
end

local originals = {
    candidate("phrase", 0, 4, "one"),
    candidate("phrase", 0, 4, "two"),
    candidate("phrase", 0, 4, "chosen"),
    candidate("phrase", 0, 4, "four"),
}
local live = run_filter(originals, {
    _ai_candidate = "chosen",
    _ai_input = "code",
    _ai_generation = "7",
})
assert(rawequal(live[1], originals[3]), "matching live result must move the real Candidate first")
same(live[2], originals[1], "first non-match order")
same(live[3], originals[2], "second non-match order")
same(live[4], originals[4], "last non-match order")
assert_originals_once(live, originals)

local duplicate_text = {
    candidate("phrase", 0, 4, "one"),
    candidate("phrase", 0, 4, "chosen"),
    candidate("phrase", 0, 4, "two"),
    candidate("phrase", 0, 4, "chosen"),
}
local duplicate_output = run_filter(duplicate_text, {
    _ai_candidate = "chosen",
    _ai_input = "code",
    _ai_generation = "7",
})
assert(rawequal(duplicate_output[1], duplicate_text[2]),
    "the first matching Candidate object must move first")
same(duplicate_output[2], duplicate_text[1], "duplicate non-match order")
same(duplicate_output[3], duplicate_text[3], "duplicate non-match order")
assert(rawequal(duplicate_output[4], duplicate_text[4]),
    "a second Candidate with the same text must not be collapsed")
assert_originals_once(duplicate_output, duplicate_text)

local generated_originals = {
    candidate("phrase", 0, 4, "one"),
    candidate("phrase", 0, 4, "two"),
}
local generated = run_filter(generated_originals, {
    _ai_candidate = "new answer",
    _ai_input = "code",
    _ai_generation = "8",
})
same(generated[1].type, "ai", "no-match result type")
same(generated[1].text, "new answer", "no-match result text")
same(generated[1].start, 0, "synthetic AI candidate start")
same(generated[1]._end, 4, "synthetic AI candidate end")
same(generated[2], generated_originals[1], "first replay after synthetic result")
same(generated[3], generated_originals[2], "second replay after synthetic result")
assert_originals_once(generated, generated_originals, 1)

local stale = run_filter(originals, {
    _ai_candidate = "chosen",
    _ai_input = "old-code",
    _ai_generation = "9",
})
for index, original in ipairs(originals) do
    assert(rawequal(stale[index], original), "stale AI metadata must pass through unchanged")
end
assert_originals_once(stale, originals)

for _, invalid in ipairs({
    {name = "missing candidate", properties = {_ai_input = "code", _ai_generation = "9"}},
    {name = "empty candidate", properties = {_ai_candidate = "", _ai_input = "code", _ai_generation = "9"}},
    {name = "missing input", properties = {_ai_candidate = "chosen", _ai_generation = "9"}},
    {name = "empty input", properties = {_ai_candidate = "chosen", _ai_input = "", _ai_generation = "9"}},
    {name = "missing generation", properties = {_ai_candidate = "chosen", _ai_input = "code"}},
    {name = "empty generation", properties = {_ai_candidate = "chosen", _ai_input = "code", _ai_generation = ""}},
}) do
    local output = run_filter(originals, invalid.properties)
    for index, original in ipairs(originals) do
        assert(rawequal(output[index], original), invalid.name .. " must pass through unchanged")
    end
    assert_originals_once(output, originals)
end

local inactive = run_filter(originals, {
    _ai_candidate = "chosen",
    _ai_input = "code",
    _ai_generation = "9",
}, "code", no_segment)
for index, original in ipairs(originals) do
    assert(rawequal(inactive[index], original), "live metadata without an active segment must pass through")
end
assert_originals_once(inactive, originals)

local boundary = {}
for index = 1, 9 do
    boundary[index] = candidate("phrase", 0, 4, index == 8 and "boundary" or ("item" .. index))
end
local boundary_output, boundary_reads = run_filter(boundary, {
    _ai_candidate = "boundary",
    _ai_input = "code",
    _ai_generation = "10",
})
assert(rawequal(boundary_output[1], boundary[8]), "a duplicate at index eight must move the real object")
same(boundary_reads, 8, "incoming reads before yielding an index-eight match")
assert_originals_once(boundary_output, boundary)

local deep = {}
for index = 1, 9 do
    deep[index] = candidate("phrase", 0, 4, index == 9 and "deep" or ("item" .. index))
end
local bounded, bounded_reads = run_filter(deep, {
    _ai_candidate = "deep",
    _ai_input = "code",
    _ai_generation = "10",
})
same(bounded[1].type, "ai", "a match after the first eight must use a synthetic Candidate")
same(bounded[1].text, "deep", "bounded synthetic text")
same(bounded_reads, 8, "incoming reads before yielding a synthetic result")
for index, original in ipairs(deep) do
    assert(rawequal(bounded[index + 1], original), "bounded filter must replay originals in order")
end
assert_originals_once(bounded, deep, 1)

local display_roundtrip_input = "hdyckclvuhxxwf"
local display_roundtrip_raw = "还要考虑上下文"
local display_roundtrip_spaced = " " .. display_roundtrip_raw

local unsafe_roundtrip_genuine = candidate("phrase", 1, 4, "Rime")
local unsafe_roundtrip_match = wrapper(
    "Shadow", "shadow", "Rime", "", unsafe_roundtrip_genuine, 0, 4
)
local safe_roundtrip_match = candidate("phrase", 0, 4, "Rime")
local safe_match_roundtrip = run_filter({
    unsafe_roundtrip_match,
    safe_roundtrip_match,
}, {
    _ai_candidate = " Rime",
    _ai_input = "code",
    _ai_generation = "safe-match-display-roundtrip",
}, "code", nil, {type = "phrase", text = "中文"})
assert(rawequal(safe_match_roundtrip[1], safe_roundtrip_match),
    "display-prefix matching must skip a candidate that downstream cannot wrap safely")
assert_originals_once(safe_match_roundtrip, {
    unsafe_roundtrip_match,
    safe_roundtrip_match,
})

local reverse_roundtrip_original = candidate("phrase", 0, 4, "Rime")
local reverse_roundtrip = run_filter({reverse_roundtrip_original}, {
    _ai_candidate = " Rime",
    _ai_input = "code",
    _ai_generation = "reverse-display-roundtrip",
}, "code", nil, {type = "phrase", text = "中文"})
assert(rawequal(reverse_roundtrip[1], reverse_roundtrip_original),
    "Han-to-ASCII display prefix must promote the original raw candidate")

local natural_spaced_roundtrip_original = candidate(
    "phrase", 0, 4, " 新答案"
)
local natural_spaced_roundtrip_raw = candidate("phrase", 0, 4, "新答案")
local natural_spaced_roundtrip = run_filter({
    natural_spaced_roundtrip_raw,
    natural_spaced_roundtrip_original,
}, {
    _ai_candidate = " 新答案",
    _ai_input = "code",
    _ai_generation = "natural-space-roundtrip",
}, "code", nil, {type = "phrase", text = "Rime"})
assert(rawequal(natural_spaced_roundtrip[1], natural_spaced_roundtrip_original),
    "an exact naturally spaced candidate must keep its original provenance")
assert_originals_once(natural_spaced_roundtrip, {
    natural_spaced_roundtrip_raw,
    natural_spaced_roundtrip_original,
})

local generated_roundtrip = run_filter({
    candidate("phrase", 0, 4, "别的候选"),
}, {
    _ai_candidate = " 新答案",
    _ai_input = "code",
    _ai_generation = "generated-display-roundtrip",
}, "code", nil, {type = "phrase", text = "Rime"})
same(generated_roundtrip[1].type, "ai",
    "unmatched display round trip must remain a synthetic AI candidate")
same(generated_roundtrip[1].text, " 新答案",
    "an unmatched leading space must remain ambiguous and fail closed")

local function assert_prefixed_ai_is_not_canonicalized(label, history_record,
        property_text, segment_start)
    local start_pos = segment_start or 0
    local active_segment = {
        start = start_pos,
        _end = #display_roundtrip_input,
        status = "selected",
    }
    local original = candidate(
        "ai_learned",
        start_pos,
        #display_roundtrip_input,
        display_roundtrip_raw,
        "AI"
    )
    local output = run_filter({original}, {
        _ai_candidate = property_text or display_roundtrip_spaced,
        _ai_input = display_roundtrip_input,
        _ai_generation = "display-roundtrip-negative",
    }, display_roundtrip_input, active_segment, history_record)

    same(output[1].type, "ai", label .. " must keep a terminal AI candidate")
    same(output[1].text, property_text or display_roundtrip_spaced,
        label .. " must preserve the live property verbatim")
    assert(not rawequal(output[1], original),
        label .. " must not borrow provenance from the raw candidate")
    assert(rawequal(output[2], original),
        label .. " must replay the original candidate unchanged")
end

assert_prefixed_ai_is_not_canonicalized("missing committed history", nil)
assert_prefixed_ai_is_not_canonicalized("raw committed history", {
    type = "raw",
    text = "Rime",
})
assert_prefixed_ai_is_not_canonicalized("thru committed history", {
    type = "thru",
    text = "Rime",
})
assert_prefixed_ai_is_not_canonicalized("non-spacing boundary", {
    type = "phrase",
    text = "中文",
})
assert_prefixed_ai_is_not_canonicalized("multiple leading spaces", {
    type = "phrase",
    text = "Rime",
}, "  " .. display_roundtrip_raw)
assert_prefixed_ai_is_not_canonicalized("non-initial segment", {
    type = "phrase",
    text = "Rime",
}, display_roundtrip_spaced, 1)

local real_execute = os.execute
local real_popen = io.popen
local real_open = io.open
local real_rename = os.rename

local function shell_quote(value)
    return "'" .. tostring(value):gsub("'", "'\"'\"'") .. "'"
end

local mktemp_command_prefix = "/usr/bin/mktemp -q "
local function assert_mktemp_command(command)
    command = tostring(command)
    assert(command:sub(1, #mktemp_command_prefix) == mktemp_command_prefix,
        "atomic temp creation must use the absolute /usr/bin/mktemp -q prefix")
end

local function command_succeeded(result)
    return result == true or result == 0
end

local function run_command(command, message)
    local result = real_execute(command)
    assert(command_succeeded(result), message or ("command failed: " .. command))
end

local function assert_file_absent(path, message)
    local file = real_open(path, "r")
    if file then
        file:close()
        error(message or ("unexpected file: " .. path))
    end
end

local temp_seed = os.tmpname()
os.remove(temp_seed)
local shell_tmpdir = assert(os.getenv("TMPDIR"), "TMPDIR is required for shell-expansion sentinels")
assert(shell_tmpdir:sub(-1) == "/", "TMPDIR must end with a slash")
local temp_token = assert(temp_seed:match("([^/]+)$"), "temporary seed must have a basename")
local dollar_sentinel_name = "rime-ai-dollar-sentinel-" .. temp_token
local backtick_sentinel_name = "rime-ai-backtick-sentinel-" .. temp_token
local dollar_sentinel_path = shell_tmpdir .. dollar_sentinel_name
local backtick_sentinel_path = shell_tmpdir .. backtick_sentinel_name
os.remove(dollar_sentinel_path)
os.remove(backtick_sentinel_path)
local temp_dir = temp_seed .. " Rime's AI weights " ..
    "$(touch${IFS}${TMPDIR}" .. dollar_sentinel_name .. ") " ..
    "`touch${IFS}${TMPDIR}" .. backtick_sentinel_name .. "`"
assert(temp_dir:find(" ", 1, true), "temporary Rime directory must contain a space")
assert(temp_dir:find("'", 1, true), "temporary Rime directory must contain a single quote")
assert(temp_dir:find("$(", 1, true),
    "temporary Rime directory must contain literal command-substitution syntax")
assert(temp_dir:find("`", 1, true), "temporary Rime directory must contain literal backticks")
run_command("/bin/mkdir " .. shell_quote(temp_dir), "failed to create temporary Rime user directory")
assert_file_absent(dollar_sentinel_path, "dollar command substitution must stay literal")
assert_file_absent(backtick_sentinel_path, "backtick command substitution must stay literal")
local created_dirs = {temp_dir}

local function make_directory(path)
    run_command("/bin/mkdir " .. shell_quote(path), "failed to create fault-injection directory")
    created_dirs[#created_dirs + 1] = path
    return path
end

local weights_path

local function read_file(path)
    local file = real_open(path, "r")
    if not file then
        return nil
    end
    local contents = file:read("*a")
    file:close()
    return contents
end

local function write_file(path, contents)
    local file = assert(real_open(path, "w"))
    assert(file:write(contents))
    assert(file:close())
end

local command_output_serial = 0
local function command_output(command)
    command_output_serial = command_output_serial + 1
    local output_path = temp_dir .. "/.test-command-output." .. command_output_serial
    os.remove(output_path)
    local result = real_execute(command .. " > " .. shell_quote(output_path))
    if not command_succeeded(result) then
        os.remove(output_path)
        return nil
    end
    local output = read_file(output_path)
    os.remove(output_path)
    return output
end

local function file_mode(path)
    local output = command_output("/usr/bin/stat -f %Lp " .. shell_quote(path))
    return output and output:match("^(%d+)\n?$") or nil
end

local function set_file_mode(path, mode)
    run_command("/bin/chmod " .. mode .. " " .. shell_quote(path),
        "failed to set fixture permissions")
end

local function ai_temp_files(directory)
    local command = table.concat({
        "/usr/bin/find",
        shell_quote(directory),
        "-maxdepth 1 -type f -name",
        shell_quote("ai_weights.tsv.tmp*"),
        "-print",
    }, " ")
    local output = assert(command_output(command), "failed to list atomic temp files")
    local paths = {}
    for path in output:gmatch("[^\n]+") do
        paths[#paths + 1] = path
    end
    return paths
end

local function assert_no_ai_temp_files(directory, message)
    local paths = ai_temp_files(directory)
    same(#paths, 0, message or "atomic write must leave no temporary files")
    same(read_file(directory .. "/ai_weights.tsv.tmp"), nil,
        "atomic write must never fall back to a fixed .tmp path")
end

local function exact_rows(text, schema_id, input, candidate_text)
    local count, weight = 0, nil
    for line in (text or ""):gmatch("[^\n]+") do
        local row_schema, row_input, row_text, row_weight, row_time =
            line:match("^([^\t\r\n]*)\t([^\t\r\n]*)\t([^\t\r\n]*)\t([^\t\r\n]*)\t([^\t\r\n]*)$")
        if row_schema == schema_id and row_input == input and row_text == candidate_text then
            count = count + 1
            weight = tonumber(row_weight)
            assert(tonumber(row_time), "learned row timestamp must be numeric")
        end
    end
    return count, weight
end

local selected_segment = {start = 2, _end = 6, status = "selected"}
local selected_candidate = candidate("ai", 2, 6, "chosen correction")
local learn_env, learn_context
local legacy_contents = "test_schema\tseed\tlegacy\t1\t1\n"
do
    local shared_home = make_directory(temp_dir .. "/shared home")
    make_directory(shared_home .. "/Library")
    local shared_directory = make_directory(shared_home .. "/Library/Rime")
    weights_path = shared_directory .. "/ai_weights.tsv"
    local user_data_dir_reads = 0
    rime_api = {
        get_user_data_dir = function()
            user_data_dir_reads = user_data_dir_reads + 1
            return temp_dir
        end,
    }
    learn_env, learn_context = env("xxcodeyy", selected_segment, "test_schema", nil,
        "~/Library/Rime/ai_weights.tsv")
    write_file(weights_path, legacy_contents)
    set_file_mode(weights_path, "0644")
    same(file_mode(weights_path), "644", "fixture must begin with broad permissions under umask 022")
    local real_getenv = os.getenv
    local home_reads = 0
    local shared_chmod_commands = {}
    os.getenv = function(name)
        if name == "HOME" then
            home_reads = home_reads + 1
            return shared_home
        end
        return real_getenv(name)
    end
    os.execute = function(command)
        if tostring(command):find("chmod", 1, true) then
            shared_chmod_commands[#shared_chmod_commands + 1] = tostring(command)
        end
        return real_execute(command)
    end
    ai_learned_translator.init(learn_env)
    os.getenv = real_getenv
    os.execute = real_execute
    same(user_data_dir_reads, 0, "configured weights path must not consult the frontend user-data directory")
    same(home_reads, 1, "configured ~/ weights path must expand HOME exactly once")
    same(learn_env.ai_weights_path, weights_path, "configured weights path expansion")
    same(#shared_chmod_commands, 1, "shared learned TSV must be secured exactly once at init")
    same(shared_chmod_commands[1], "/bin/chmod 600 " .. shell_quote(weights_path),
        "shared learned TSV chmod path")
    same(file_mode(weights_path), "600", "translator init must secure an existing learned TSV")
    same(learn_context.select_notifier.slots[1].group, 0, "select notifier group")
    same(learn_context.commit_notifier.slots[1].group, 0, "commit notifier group")
    same(learn_context.update_notifier.slots[1].group, 0, "update notifier group")

    local invalid_storage_accesses = 0
    local function unexpected_storage_io()
        invalid_storage_accesses = invalid_storage_accesses + 1
        return nil
    end
    rime_api.get_user_data_dir = unexpected_storage_io
    io.open = unexpected_storage_io
    io.popen = unexpected_storage_io
    os.execute = unexpected_storage_io
    os.rename = unexpected_storage_io
    for _, invalid in ipairs({
        {name = "relative", path = "relative/ai_weights.tsv"},
        {name = "other user", path = "~other/ai_weights.tsv"},
        {name = "literal HOME", path = "$HOME/ai_weights.tsv"},
        {name = "NUL", path = "/tmp/ai\0weights.tsv"},
        {name = "CR", path = "/tmp/ai\rweights.tsv"},
        {name = "LF", path = "/tmp/ai\nweights.tsv"},
    }) do
        local invalid_env = env("code", {start = 0, _end = 4}, "test_schema", nil, invalid.path)
        ai_learned_translator.init(invalid_env)
        same(invalid_env.ai_weights_path, invalid.path,
            invalid.name .. " configured path must not fall back")
        same(invalid_env.ai_storage_ready, false,
            invalid.name .. " configured path must fail closed")
        ai_learned_translator.fini(invalid_env)
    end
    io.open = real_open
    io.popen = real_popen
    os.execute = real_execute
    os.rename = real_rename
    same(invalid_storage_accesses, 0, "invalid configured paths must perform no storage I/O")
    rime_api.get_user_data_dir = function()
        return shared_directory
    end
end

local renamed_temp_basenames = {}
local renamed_temp_seen = {}
local atomic_temp_open_count = 0
local successful_mktemp_commands = {}
io.popen = function(command, mode)
    if tostring(command):find("mktemp", 1, true) then
        assert_mktemp_command(command)
        successful_mktemp_commands[#successful_mktemp_commands + 1] = tostring(command)
    end
    return real_popen(command, mode)
end
io.open = function(path, mode)
    if type(path) == "string" and path:find("ai_weights.tsv", 1, true) then
        assert(path == weights_path or path:sub(1, #weights_path + 5) == weights_path .. ".tmp.",
            "main learned storage must use only the expanded shared path")
    end
    if type(path) == "string" and path:match("/ai_weights%.tsv%.tmp%..+$") then
        atomic_temp_open_count = atomic_temp_open_count + 1
        same(mode, "r+", "mktemp output must be opened without truncating or recreating it")
    end
    return real_open(path, mode)
end
os.rename = function(source, destination)
    if source ~= destination and type(destination) == "string" and
        destination:find("ai_weights.tsv", 1, true) then
        same(destination, weights_path, "atomic replace destination must be the shared path")
    end
    if destination == weights_path and source ~= destination then
        local basename = source:match("/([^/]+)$")
        assert(basename and basename:match("^ai_weights%.tsv%.tmp%..+$"),
            "atomic source must use a unique ai_weights.tsv.tmp.* basename")
        same(file_mode(source), "600", "atomic source must be mode 0600 before rename")
        assert(not renamed_temp_seen[basename], "atomic temp basename must be unique per write")
        renamed_temp_seen[basename] = true
        renamed_temp_basenames[#renamed_temp_basenames + 1] = basename
    end
    return real_rename(source, destination)
end

do
    local punct = {
        segment = {start = 0, _end = 1, status = "selected"},
        candidate = candidate("punct", 0, 1, "，"),
        contents = legacy_contents .. "test_schema\t,\t，\t1\t1\n",
    }
    function punct.segment:has_tag(tag)
        return tag == "punct"
    end

    punct.live = run_filter({punct.candidate}, {
        _ai_candidate = "可以，",
        _ai_input = ",",
        _ai_generation = "punct-guard",
    }, ",", punct.segment)
    same(#punct.live, 1, "punct live candidate count")
    assert(rawequal(punct.live[1], punct.candidate),
        "punct live filter must pass through the original native candidate")

    write_file(weights_path, punct.contents)
    yielded = {}
    ai_learned_translator.func(",", punct.segment, learn_env)
    same(#yielded, 0, "punct learned candidate count")

    learn_context.input = ","
    learn_context.composition.segment = punct.segment
    learn_context.properties._ai_generation = "punct-guard"
    learn_context.selected_candidate = punct.candidate
    learn_context.select_notifier:emit(learn_context)
    learn_context.commit_notifier:emit(learn_context)
    same(read_file(weights_path), punct.contents,
        "punct select and commit must not change learned storage")

    punct.candidate = candidate("punct", 0, 1, ".")
    punct.contents = legacy_contents .. "test_schema\t.\t。\t1\t1\n"
    function punct.segment:has_tag(tag)
        return tag == "punct_number"
    end

    punct.live = run_filter({punct.candidate}, {
        _ai_candidate = "。",
        _ai_input = ".",
        _ai_generation = "punct-number-guard",
    }, ".", punct.segment)
    same(#punct.live, 1, "punct_number live candidate count")
    assert(rawequal(punct.live[1], punct.candidate),
        "punct_number live filter must pass through the native decimal separator")

    write_file(weights_path, punct.contents)
    yielded = {}
    ai_learned_translator.func(".", punct.segment, learn_env)
    same(#yielded, 0, "punct_number learned candidate count")

    learn_context.input = "."
    learn_context.composition.segment = punct.segment
    learn_context.properties._ai_generation = "punct-number-guard"
    learn_context.selected_candidate = punct.candidate
    learn_context.select_notifier:emit(learn_context)
    learn_context.commit_notifier:emit(learn_context)
    same(read_file(weights_path), punct.contents,
        "punct_number select and commit must not change learned storage")

    write_file(weights_path, legacy_contents)
    learn_context.input = "xxcodeyy"
    learn_context.composition.segment = selected_segment
end

learn_context.properties._ai_generation = ""
learn_context.selected_candidate = selected_candidate
learn_context.select_notifier:emit(learn_context, function()
    learn_context.input = "mutated"
    selected_segment.start = 0
    selected_segment._end = 7
    selected_segment.status = "guess"
    selected_candidate.type = "phrase"
    selected_candidate.text = "wrong late value"
    learn_env.engine.schema.schema_id = "wrong_schema"
end)
same(read_file(weights_path), legacy_contents,
    "selection must preserve legacy contents and not persist before commit")
learn_context.input = "xxcodeyy"
learn_context.update_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
local learned = read_file(weights_path)
same(file_mode(weights_path), "600", "committed learned TSV must remain mode 0600")
local learned_count, learned_weight = exact_rows(learned, "test_schema", "code", "chosen correction")
same(learned_count, 1, "group 0 snapshot exact row count")
same(learned_weight, 1, "group 0 snapshot weight")
assert(not learned:find("wrong", 1, true), "learning must not retain objects mutated by later callbacks")
learn_context.commit_notifier:emit(learn_context)
same(read_file(weights_path), learned, "commit without a new selection must not write or increment again")

learn_env.engine.schema.schema_id = "test_schema"
learn_context.input = "xxcodeyy"
learn_context.composition.segment = {start = 2, _end = 6, status = "selected"}
learn_context.properties._ai_generation = ""
learn_context.selected_candidate = candidate("ai_learned", 2, 6, "chosen correction")
learn_context.select_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
learned_count, learned_weight = exact_rows(read_file(weights_path),
    "test_schema", "code", "chosen correction")
same(learned_count, 1, "incremented learned key row count")
same(learned_weight, 2, "committing a learned candidate must increment its independent weight")

learn_context.input = "code"
learn_context.composition.segment = {start = 0, _end = 4, status = "selected"}
local spaced_ultimate = candidate("ai_learned", 0, 4, "spaced correction")
local spaced_final = wrapper(
    "Uniquified", "uniquified", "Chosen Display", "final-comment", spaced_ultimate
)
local spaced_duplicate_ultimate = candidate(
    "ai_learned", 0, 4, "spaced correction duplicate"
)
local spaced_duplicate_final = wrapper(
    "Uniquified",
    "uniquified",
    "Chosen Display",
    "duplicate-comment",
    spaced_duplicate_ultimate
)
local spaced_menu = run_spacing_pipeline("中文", {
    spaced_final,
    spaced_duplicate_final,
})
same(#spaced_menu, 1, "AI integration final uniquifier menu count")
local spaced = spaced_menu[1]
same(spaced.type, "auto_space", "AI integration must exercise auto spacing")
same(spaced.text, " Chosen Display", "AI integration spaced display")
same(spaced:get_dynamic_type(), "Uniquified",
    "AI integration post-uniquifier dynamic type")
assert(rawequal(spaced:get_genuine(), spaced_ultimate),
    "AI integration must expose ultimate genuine")
local spaced_genuines = spaced:get_genuines()
same(#spaced_genuines, 2, "AI integration flat genuine count")
assert(rawequal(spaced_genuines[1], spaced_ultimate),
    "AI integration first flat genuine")
assert(rawequal(spaced_genuines[2], spaced_duplicate_ultimate),
    "AI integration duplicate flat genuine")

learn_context.selected_candidate = spaced
learn_context.properties._ai_generation = ""
learn_context.select_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
local spaced_count, spaced_weight = exact_rows(
    read_file(weights_path), "test_schema", "code", "spaced correction"
)
same(spaced_count, 1, "AI learning must persist ultimate genuine text")
same(spaced_weight, 1, "AI learning ultimate genuine weight")

local spaced_display_count = exact_rows(
    read_file(weights_path), "test_schema", "code", " Chosen Display"
)
same(spaced_display_count, 0, "spaced display text must not enter AI learning")

write_file(weights_path, read_file(weights_path) .. table.concat({
    "test_schema",
    display_roundtrip_input,
    display_roundtrip_raw,
    "1",
    "1",
}, "\t") .. "\n")
local roundtrip_raw_count, roundtrip_raw_weight = exact_rows(
    read_file(weights_path),
    "test_schema",
    display_roundtrip_input,
    display_roundtrip_raw
)
same(roundtrip_raw_count, 1, "display-roundtrip fixture raw row count")
same(roundtrip_raw_weight, 1, "display-roundtrip fixture raw weight")
local roundtrip_spaced_count = exact_rows(
    read_file(weights_path),
    "test_schema",
    display_roundtrip_input,
    display_roundtrip_spaced
)
same(roundtrip_spaced_count, 0, "display-roundtrip fixture spaced row count")

local roundtrip_history = {type = "return_raw", text = "Rime"}
local roundtrip_segment = {
    start = 0,
    _end = #display_roundtrip_input,
    status = "selected",
}
local roundtrip_original = candidate(
    "ai_learned",
    roundtrip_segment.start,
    roundtrip_segment._end,
    display_roundtrip_raw,
    "AI",
    1.201
)
local roundtrip_duplicate = candidate(
    "phrase",
    roundtrip_segment.start,
    roundtrip_segment._end,
    display_roundtrip_raw
)
local roundtrip_filtered = run_filter({roundtrip_original, roundtrip_duplicate}, {
    _ai_candidate = display_roundtrip_spaced,
    _ai_input = display_roundtrip_input,
    _ai_generation = "display-roundtrip",
}, display_roundtrip_input, roundtrip_segment, roundtrip_history)
local roundtrip_menu = run_spacing_pipeline(roundtrip_history, roundtrip_filtered)
same(roundtrip_menu[1].text, display_roundtrip_spaced,
    "display-roundtrip final display must contain exactly one boundary space")

local roundtrip_env, roundtrip_context = env(
    display_roundtrip_input,
    roundtrip_segment,
    "test_schema"
)
function roundtrip_segment:get_selected_candidate()
    return self.selected_candidate
end
function roundtrip_context:get_selected_candidate()
    local selected_segment = self.composition:back()
    return selected_segment and selected_segment:get_selected_candidate() or nil
end
roundtrip_segment.selected_candidate = roundtrip_menu[1]
roundtrip_context.properties._ai_generation = "display-roundtrip"
ai_learned_translator.init(roundtrip_env)
roundtrip_context.select_notifier:emit(roundtrip_context)
roundtrip_context.commit_notifier:emit(roundtrip_context)

roundtrip_raw_count, roundtrip_raw_weight = exact_rows(
    read_file(weights_path),
    "test_schema",
    display_roundtrip_input,
    display_roundtrip_raw
)
same(roundtrip_raw_count, 1,
    "display-prefixed AI must keep one canonical raw learned row")
same(roundtrip_raw_weight, 2,
    "display-prefixed AI must increment the canonical raw learned weight")
roundtrip_spaced_count = exact_rows(
    read_file(weights_path),
    "test_schema",
    display_roundtrip_input,
    display_roundtrip_spaced
)
same(roundtrip_spaced_count, 0,
    "display-prefixed AI must not create a leading-space learned row")
same(#roundtrip_menu, 1, "display-roundtrip final uniquifier menu count")
same(roundtrip_menu[1].type, "auto_space",
    "display-roundtrip final candidate must retain auto-space provenance")
same(roundtrip_menu[1]:get_dynamic_type(), "Uniquified",
    "display-roundtrip final candidate must have native uniquifier shape")
assert(rawequal(roundtrip_menu[1]:get_genuine(), roundtrip_original),
    "display-roundtrip final candidate genuine must be the original raw candidate")
ai_learned_translator.fini(roundtrip_env)

learn_context.input = "native"
learn_context.composition.segment = {start = 0, _end = 6, status = "selected"}
learn_context.properties._ai_generation = "13"
learn_context.selected_candidate = candidate("phrase", 0, 6, "native correction")
learn_context.select_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
local native_count, native_weight = exact_rows(read_file(weights_path),
    "test_schema", "native", "native correction")
same(native_count, 1, "native correction exact row count")
same(native_weight, 1, "native candidate selected under a live generation must be learned")
io.popen = real_popen
io.open = real_open
os.rename = real_rename
assert(#renamed_temp_basenames >= 3, "expected several independent atomic learning writes")
same(atomic_temp_open_count, #renamed_temp_basenames,
    "each renamed temp file must be opened exactly once in r+ mode")
same(#successful_mktemp_commands, #renamed_temp_basenames,
    "each successful atomic write must invoke absolute /usr/bin/mktemp -q once")
for _, command in ipairs(successful_mktemp_commands) do
    same(command, "/usr/bin/mktemp -q " .. shell_quote(weights_path .. ".tmp.XXXXXX"),
        "atomic temp creation must use the shared path")
end

local function assert_aborted_selection_not_written(candidate_text, abort)
    learn_context.input = "code"
    learn_context.composition.segment = {start = 0, _end = 4, status = "selected"}
    learn_context.properties._ai_generation = "12"
    learn_context.selected_candidate = candidate("ai", 0, 4, candidate_text)
    learn_context.select_notifier:emit(learn_context)
    abort()
    learn_context.update_notifier:emit(learn_context)
    learn_context.commit_notifier:emit(learn_context)
    assert(not read_file(weights_path):find(candidate_text, 1, true),
        "aborted selection must not be learned: " .. candidate_text)
end

assert_aborted_selection_not_written("cancelled", function()
    learn_context.input = ""
    learn_context.composition.segment = nil
end)
assert_aborted_selection_not_written("changed-input", function()
    learn_context.input = "other"
    learn_context.composition.segment = {start = 0, _end = 5, status = "selected"}
end)
assert_aborted_selection_not_written("reopened-span", function()
    learn_context.composition.segment = {start = 0, _end = 4, status = "guess"}
end)

learn_context.input = "plain"
learn_context.composition.segment = {start = 0, _end = 5, status = "selected"}
learn_context.properties._ai_generation = ""
learn_context.selected_candidate = candidate("phrase", 0, 5, "ordinary")
learn_context.select_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
assert(not read_file(weights_path):find("ordinary", 1, true),
    "ordinary selections without a live generation must not enter the AI lexicon")

os.remove(weights_path)
yielded = {}
ai_learned_translator.func("code", {start = 2, _end = 6}, learn_env)
same(#yielded, 0, "missing learned TSV must yield no candidates")

write_file(weights_path, table.concat({
    "test_schema\tcode\tdelta\t3\t50",
    "test_schema\tcode\tgamma\t2\t200",
    "test_schema\tcode\tbeta\t2\t100",
    "test_schema\tcode\talpha\t2\t100",
    "test_schema\tother\tignored-input\t9\t9",
    "other-schema\tcode\tignored-schema\t9\t9",
    "test_schema\tcode\tbad-weight\tnope\t1",
    "test_schema\tcode\tnegative-weight\t-1\t1",
    "test_schema\tcode\tnegative-time\t1\t-1",
    "\tcode\tempty-schema\t9\t9",
    "test_schema\t\tempty-input\t9\t9",
    "test_schema\tcode\t\t9\t9",
    "test_schema\tcode\tbad\textra\t1\t1",
    "test_schema\tcode\tbad\rtext\t1\t1",
    "test_schema\tcode\tbroken\nfield\t1\t1",
    "short\trow",
}, "\n") .. "\n")
yielded = {}
ai_learned_translator.func("code", {start = 2, _end = 6}, learn_env)
same(#yielded, 4, "only valid matching learned rows should be yielded")
same(yielded[1].text, "delta", "learned weight ordering")
same(yielded[2].text, "gamma", "learned timestamp ordering")
same(yielded[3].text, "alpha", "learned text tie-break ordering")
same(yielded[4].text, "beta", "learned text tie-break ordering")
for _, value in ipairs(yielded) do
    same(value.type, "ai_learned", "learned candidate type")
    same(value.start, 2, "learned candidate start")
    same(value._end, 6, "learned candidate end")
    assert(value.quality > 1.2, "learned quality must overlay the native translator baseline")
    assert(value.quality <= 1.3, "learned quality boost must stay small and bounded")
end
assert(yielded[1].quality > yielded[2].quality, "learned quality must preserve weight ordering")
same(yielded[2].quality, yielded[3].quality, "equal weights must receive equal quality boosts")

for _, invalid_key in ipairs({
    {name = "empty schema", schema_id = "", input = "code"},
    {name = "empty input", schema_id = "test_schema", input = ""},
}) do
    local invalid_env = env(invalid_key.input, {start = 0, _end = 4}, invalid_key.schema_id)
    yielded = {}
    ai_learned_translator.func(invalid_key.input, {start = 0, _end = 4}, invalid_env)
    same(#yielded, 0, invalid_key.name .. " learned row must be ignored")
end

for _, invalid_quality in ipairs({false, "not-a-number"}) do
    local safe_quality_env = env("code", {start = 0, _end = 4}, "test_schema", invalid_quality)
    yielded = {}
    ai_learned_translator.func("code", {start = 0, _end = 4}, safe_quality_env)
    assert(#yielded > 0 and yielded[1].quality > 0,
        "missing or non-numeric initial quality must use a safe baseline")
end

local fault_serial = 0
local function storage_case(label, mode)
    fault_serial = fault_serial + 1
    local directory = make_directory(temp_dir .. "/" .. label .. " " .. fault_serial)
    local path = directory .. "/ai_weights.tsv"
    local original = "test_schema\tfault-input\tpreserved\t1\t1\n"
    write_file(path, original)
    set_file_mode(path, mode or "0600")
    rime_api.get_user_data_dir = function()
        return directory
    end
    local case_env, case_context = env("fault-input",
        {start = 0, _end = 11, status = "selected"}, "test_schema", nil, "")
    ai_learned_translator.init(case_env)
    return {
        directory = directory,
        path = path,
        original = original,
        env = case_env,
        context = case_context,
    }
end

local function trigger_learning(case, text)
    local case_context = case.context
    case_context.input = "fault-input"
    case_context.composition.segment = {start = 0, _end = 11, status = "selected"}
    case_context.properties._ai_generation = "fault-generation"
    case_context.selected_candidate = candidate("ai", 0, 11, text)
    case_context.select_notifier:emit(case_context)
    case_context.commit_notifier:emit(case_context)
end

local function assert_storage_preserved(case, message)
    same(read_file(case.path), case.original, message .. " must preserve the target")
    assert_no_ai_temp_files(case.directory, message .. " must clean every temp file")
end

local function is_case_temp(case, path)
    return type(path) == "string" and path:sub(1, #case.directory + 1) == case.directory .. "/" and
        path:match("/ai_weights%.tsv%.tmp%..+$") ~= nil
end

-- Creating the lexicon from scratch must also preserve mktemp's 0600 mode.
local new_file_directory = make_directory(temp_dir .. "/new learned file")
local new_file_path = new_file_directory .. "/ai_weights.tsv"
rime_api.get_user_data_dir = function()
    return new_file_directory
end
local new_file_env, new_file_context = env("fault-input",
    {start = 0, _end = 11, status = "selected"}, "test_schema")
ai_learned_translator.init(new_file_env)
local new_file_case = {
    directory = new_file_directory,
    path = new_file_path,
    env = new_file_env,
    context = new_file_context,
}
local new_file_open_count, new_file_rename_count = 0, 0
io.open = function(path, mode)
    if is_case_temp(new_file_case, path) then
        new_file_open_count = new_file_open_count + 1
        same(mode, "r+", "new lexicon temp must use only r+ mode")
    end
    return real_open(path, mode)
end
os.rename = function(source, destination)
    if destination == new_file_path and source ~= destination then
        new_file_rename_count = new_file_rename_count + 1
        assert(is_case_temp(new_file_case, source),
            "new lexicon must use a unique ai_weights.tsv.tmp.* source")
        same(file_mode(source), "600", "new lexicon source must be mode 0600 before rename")
    end
    return real_rename(source, destination)
end
trigger_learning(new_file_case, "brand-new learned choice")
io.open = real_open
os.rename = real_rename
same(new_file_open_count, 1, "new lexicon must open exactly one unique temp")
same(new_file_rename_count, 1, "new lexicon must atomically rename exactly once")
same(file_mode(new_file_path), "600", "newly committed learned TSV must be mode 0600")
local new_file_count, new_file_weight = exact_rows(read_file(new_file_path),
    "test_schema", "fault-input", "brand-new learned choice")
same(new_file_count, 1, "new learned TSV exact row count")
same(new_file_weight, 1, "new learned TSV initial weight")
assert_no_ai_temp_files(new_file_directory, "new lexicon commit")
ai_learned_translator.fini(new_file_env)

-- Existing storage must be a regular non-symlink file before chmod can touch it.
local directory_target_parent = make_directory(temp_dir .. "/directory target")
local directory_target_path = make_directory(directory_target_parent .. "/ai_weights.tsv")
same(file_mode(directory_target_path), "755", "directory target fixture mode")
rime_api.get_user_data_dir = function()
    return directory_target_parent
end
local directory_target_env = env("fault-input",
    {start = 0, _end = 11, status = "selected"}, "test_schema")
local directory_target_chmods = 0
os.execute = function(command)
    if tostring(command):find("chmod", 1, true) then
        directory_target_chmods = directory_target_chmods + 1
    end
    return real_execute(command)
end
ai_learned_translator.init(directory_target_env)
os.execute = real_execute
same(directory_target_chmods, 0, "directory storage target must be rejected before chmod")
same(file_mode(directory_target_path), "755", "directory storage target mode must stay unchanged")
yielded = {}
ai_learned_translator.func("fault-input", {start = 0, _end = 11}, directory_target_env)
same(#yielded, 0, "directory storage target must disable learned candidates")
ai_learned_translator.fini(directory_target_env)

local symlink_target_directory = make_directory(temp_dir .. "/symlink target")
local symlink_weights_path = symlink_target_directory .. "/ai_weights.tsv"
local innocent_target_path = symlink_target_directory .. "/innocent.tsv"
local innocent_contents = "must remain untouched\n"
write_file(innocent_target_path, innocent_contents)
set_file_mode(innocent_target_path, "0644")
run_command("/bin/ln -s " .. shell_quote(innocent_target_path) .. " " ..
    shell_quote(symlink_weights_path), "failed to create learned-storage symlink fixture")
rime_api.get_user_data_dir = function()
    return symlink_target_directory
end
local symlink_target_env = env("fault-input",
    {start = 0, _end = 11, status = "selected"}, "test_schema")
local symlink_target_chmods = 0
os.execute = function(command)
    if tostring(command):find("chmod", 1, true) then
        symlink_target_chmods = symlink_target_chmods + 1
    end
    return real_execute(command)
end
ai_learned_translator.init(symlink_target_env)
os.execute = real_execute
same(symlink_target_chmods, 0, "symlink storage target must be rejected before chmod")
same(file_mode(innocent_target_path), "644", "symlink target mode must stay unchanged")
same(read_file(innocent_target_path), innocent_contents, "symlink target contents must stay unchanged")
yielded = {}
ai_learned_translator.func("fault-input", {start = 0, _end = 11}, symlink_target_env)
same(#yielded, 0, "symlink storage target must disable learned candidates")
ai_learned_translator.fini(symlink_target_env)
os.remove(symlink_weights_path)
os.remove(innocent_target_path)

local fifo_target_directory = make_directory(temp_dir .. "/fifo target")
local fifo_target_path = fifo_target_directory .. "/ai_weights.tsv"
run_command("/usr/bin/mkfifo " .. shell_quote(fifo_target_path),
    "failed to create learned-storage FIFO fixture")
rime_api.get_user_data_dir = function()
    return fifo_target_directory
end
local fifo_target_env = env("fault-input",
    {start = 0, _end = 11, status = "selected"}, "test_schema")
local fifo_open_attempts = 0
io.open = function(path, mode)
    if path == fifo_target_path then
        fifo_open_attempts = fifo_open_attempts + 1
        return nil, "special storage target must be rejected before open"
    end
    return real_open(path, mode)
end
ai_learned_translator.init(fifo_target_env)
io.open = real_open
same(fifo_open_attempts, 0, "FIFO storage target must be rejected before io.open")
yielded = {}
ai_learned_translator.func("fault-input", {start = 0, _end = 11}, fifo_target_env)
same(#yielded, 0, "FIFO storage target must disable learned candidates")
ai_learned_translator.fini(fifo_target_env)
os.remove(fifo_target_path)

-- A chmod failure is a fail-closed storage boundary for this translator instance.
local chmod_directory = make_directory(temp_dir .. "/chmod failure")
local chmod_path = chmod_directory .. "/ai_weights.tsv"
local chmod_original = "test_schema\tfault-input\tmust-not-be-read\t9\t9\n"
write_file(chmod_path, chmod_original)
set_file_mode(chmod_path, "0644")
rime_api.get_user_data_dir = function()
    return chmod_directory
end
local chmod_env, chmod_context = env("fault-input",
    {start = 0, _end = 11, status = "selected"}, "test_schema")
local chmod_commands = {}
os.execute = function(command)
    if tostring(command):find("chmod", 1, true) then
        chmod_commands[#chmod_commands + 1] = tostring(command)
        return nil, "exit", 1
    end
    return real_execute(command)
end
ai_learned_translator.init(chmod_env)
os.execute = real_execute
assert(#chmod_commands > 0, "translator init must try to secure an existing learned TSV")
for _, command in ipairs(chmod_commands) do
    assert(command:sub(1, #"/bin/chmod 600 ") == "/bin/chmod 600 ",
        "learned TSV chmod must use the absolute /bin/chmod 600 prefix")
end
same(file_mode(chmod_path), "644", "mocked chmod failure must leave fixture mode unchanged")

local disabled_accesses = 0
local disabled_mktemp_attempts = 0
io.open = function(path, mode)
    if type(path) == "string" and path:sub(1, #chmod_directory + 1) == chmod_directory .. "/" then
        disabled_accesses = disabled_accesses + 1
        return nil, "disabled storage must not open files"
    end
    return real_open(path, mode)
end
io.popen = function(command, mode)
    if tostring(command):find("mktemp", 1, true) then
        assert_mktemp_command(command)
        disabled_mktemp_attempts = disabled_mktemp_attempts + 1
        return nil, "disabled storage must not create temp files"
    end
    return real_popen(command, mode)
end
yielded = {}
ai_learned_translator.func("fault-input", {start = 0, _end = 11}, chmod_env)
same(#yielded, 0, "disabled learned storage must yield no candidates")
local chmod_case = {
    directory = chmod_directory,
    path = chmod_path,
    original = chmod_original,
    env = chmod_env,
    context = chmod_context,
}
trigger_learning(chmod_case, "must-not-be-written")
io.open = real_open
io.popen = real_popen
same(disabled_accesses, 0, "disabled learned storage must not read or write its target")
same(disabled_mktemp_attempts, 0, "disabled learned storage must not attempt an atomic write")
same(read_file(chmod_path), chmod_original, "disabled learned storage must preserve its target")
same(file_mode(chmod_path), "644", "disabled storage must not retry chmod behind the env boundary")
assert_no_ai_temp_files(chmod_directory, "disabled learned storage")
local live_during_storage_failure = run_filter(originals, {
    _ai_candidate = "chosen",
    _ai_input = "code",
    _ai_generation = "storage-failure",
})
assert(rawequal(live_during_storage_failure[1], originals[3]),
    "live AI candidate filtering must survive learned-storage failure")
ai_learned_translator.fini(chmod_env)

-- A chmod failure disables only that env; a fresh healthy env must still read and write securely.
local healthy_case = storage_case("healthy after chmod failure")
yielded = {}
ai_learned_translator.func("fault-input", {start = 0, _end = 11}, healthy_case.env)
same(#yielded, 1, "fresh env after chmod failure must read its learned TSV")
same(yielded[1].text, "preserved", "fresh env learned candidate text")
local healthy_mktemp_calls = 0
io.popen = function(command, mode)
    if tostring(command):find("mktemp", 1, true) then
        assert_mktemp_command(command)
        healthy_mktemp_calls = healthy_mktemp_calls + 1
    end
    return real_popen(command, mode)
end
trigger_learning(healthy_case, "healthy-after-chmod-failure")
io.popen = real_popen
same(healthy_mktemp_calls, 1, "fresh env must perform one secure atomic write")
same(file_mode(healthy_case.path), "600", "fresh env target must remain mode 0600")
local healthy_count, healthy_weight = exact_rows(read_file(healthy_case.path),
    "test_schema", "fault-input", "healthy-after-chmod-failure")
same(healthy_count, 1, "fresh env learned write exact row count")
same(healthy_weight, 1, "fresh env learned write initial weight")
assert_no_ai_temp_files(healthy_case.directory, "fresh env after chmod failure")
ai_learned_translator.fini(healthy_case.env)

-- mktemp failure must not fall back to a predictable filename.
local mktemp_case = storage_case("mktemp failure")
local mktemp_calls, fallback_write_attempts = 0, 0
io.popen = function(command, mode)
    if tostring(command):find("mktemp", 1, true) then
        assert_mktemp_command(command)
        mktemp_calls = mktemp_calls + 1
        return nil, "mock mktemp failure"
    end
    return real_popen(command, mode)
end
io.open = function(path, mode)
    if type(path) == "string" and path:sub(1, #mktemp_case.directory + 1) ==
        mktemp_case.directory .. "/" and mode ~= "r" then
        fallback_write_attempts = fallback_write_attempts + 1
    end
    return real_open(path, mode)
end
trigger_learning(mktemp_case, "mktemp-must-fail")
io.popen = real_popen
io.open = real_open
assert(mktemp_calls > 0, "atomic writer must use mktemp for an unpredictable basename")
same(fallback_write_attempts, 0, "mktemp failure must not use an unsafe write fallback")
assert_storage_preserved(mktemp_case, "mktemp failure")
ai_learned_translator.fini(mktemp_case.env)

-- Treat an unexpected mktemp result as untrusted; never open or rename it.
local invalid_path_case = storage_case("invalid mktemp path")
local invalid_path = temp_dir .. "/unsafe mktemp result"
os.remove(invalid_path)
local invalid_popen_calls, invalid_first_line_reads, invalid_open_attempts = 0, 0, 0
io.popen = function(command, mode)
    if tostring(command):find("mktemp", 1, true) then
        assert_mktemp_command(command)
        invalid_popen_calls = invalid_popen_calls + 1
        return {
            read = function(_, format)
                if format == "*l" then
                    invalid_first_line_reads = invalid_first_line_reads + 1
                    return invalid_path
                end
                if format == "*a" then
                    return ""
                end
                error("unexpected mktemp pipe read format: " .. tostring(format))
            end,
            close = function()
                return true
            end,
        }
    end
    return real_popen(command, mode)
end
io.open = function(path, mode)
    if path == invalid_path then
        invalid_open_attempts = invalid_open_attempts + 1
    end
    return real_open(path, mode)
end
trigger_learning(invalid_path_case, "invalid-path-must-fail")
io.popen = real_popen
io.open = real_open
assert(invalid_popen_calls > 0, "invalid-path test must intercept mktemp")
assert(invalid_first_line_reads > 0, "atomic writer must read the mktemp path as one line")
same(invalid_open_attempts, 0, "writer must reject a mktemp path outside its validated template")
same(read_file(invalid_path), nil, "invalid mktemp result must never be created")
assert_storage_preserved(invalid_path_case, "invalid mktemp path")
ai_learned_translator.fini(invalid_path_case.env)

-- A path can match the template and still be unsafe if its file is not the empty mktemp result.
local nonempty_case = storage_case("nonempty mktemp file")
local nonempty_path = nonempty_case.directory .. "/ai_weights.tsv.tmp.STALE-output"
write_file(nonempty_path, "unexpected preexisting contents\n")
set_file_mode(nonempty_path, "0600")
local nonempty_popen_calls, nonempty_first_line_reads = 0, 0
local nonempty_open_count, nonempty_write_attempts = 0, 0
io.popen = function(command, mode)
    if tostring(command):find("mktemp", 1, true) then
        assert_mktemp_command(command)
        nonempty_popen_calls = nonempty_popen_calls + 1
        return {
            read = function(_, format)
                if format == "*l" then
                    nonempty_first_line_reads = nonempty_first_line_reads + 1
                    return nonempty_path
                end
                if format == "*a" then
                    return ""
                end
                error("unexpected mktemp pipe read format: " .. tostring(format))
            end,
            close = function()
                return true
            end,
        }
    end
    return real_popen(command, mode)
end
io.open = function(path, mode)
    if path == nonempty_path then
        nonempty_open_count = nonempty_open_count + 1
        same(mode, "r+", "nonempty mktemp result must be inspected without truncation")
        local backing = assert(real_open(path, mode))
        return {
            seek = function(_, ...)
                return backing:seek(...)
            end,
            write = function(_, ...)
                nonempty_write_attempts = nonempty_write_attempts + 1
                return backing:write(...)
            end,
            close = function()
                return backing:close()
            end,
        }
    end
    return real_open(path, mode)
end
trigger_learning(nonempty_case, "nonempty-must-fail")
io.popen = real_popen
io.open = real_open
assert(nonempty_popen_calls > 0, "nonempty test must intercept mktemp")
assert(nonempty_first_line_reads > 0, "nonempty test must return the mktemp path as one line")
same(nonempty_open_count, 1, "writer must inspect the nonempty mktemp result exactly once")
same(nonempty_write_attempts, 0, "writer must reject a nonempty mktemp result before writing")
assert_storage_preserved(nonempty_case, "nonempty mktemp result")
ai_learned_translator.fini(nonempty_case.env)

-- Each I/O boundary must preserve the old target and clean the unique temp file.
local open_failure_case = storage_case("open failure")
local temp_open_attempts, unsafe_open_modes = 0, 0
io.open = function(path, mode)
    if is_case_temp(open_failure_case, path) then
        if mode == "r+" then
            temp_open_attempts = temp_open_attempts + 1
            return nil, "mock temp open failure"
        end
        unsafe_open_modes = unsafe_open_modes + 1
        return nil, "unsafe temp open mode"
    end
    return real_open(path, mode)
end
trigger_learning(open_failure_case, "open-must-fail")
io.open = real_open
assert(temp_open_attempts > 0, "open failure test must intercept the unique temp file")
same(unsafe_open_modes, 0, "atomic writer must not fall back to w or w+ after r+ open failure")
assert_storage_preserved(open_failure_case, "temp open failure")
ai_learned_translator.fini(open_failure_case.env)

local write_failure_case = storage_case("write failure")
local temp_write_attempts, unsafe_write_open_modes = 0, 0
io.open = function(path, mode)
    if is_case_temp(write_failure_case, path) and mode == "r+" then
        local backing = assert(real_open(path, mode))
        return {
            seek = function(_, ...)
                return backing:seek(...)
            end,
            write = function()
                temp_write_attempts = temp_write_attempts + 1
                return nil, "mock temp write failure"
            end,
            close = function()
                return backing:close()
            end,
        }
    elseif is_case_temp(write_failure_case, path) then
        unsafe_write_open_modes = unsafe_write_open_modes + 1
        return nil, "unsafe temp open mode"
    end
    return real_open(path, mode)
end
trigger_learning(write_failure_case, "write-must-fail")
io.open = real_open
assert(temp_write_attempts > 0, "write failure test must intercept a temp write")
same(unsafe_write_open_modes, 0, "temp write path must use only r+ mode")
assert_storage_preserved(write_failure_case, "temp write failure")
ai_learned_translator.fini(write_failure_case.env)

local close_failure_case = storage_case("close failure")
local temp_close_attempts, unsafe_close_open_modes = 0, 0
io.open = function(path, mode)
    if is_case_temp(close_failure_case, path) and mode == "r+" then
        local backing = assert(real_open(path, mode))
        return {
            seek = function(_, ...)
                return backing:seek(...)
            end,
            write = function(_, ...)
                return backing:write(...)
            end,
            close = function()
                temp_close_attempts = temp_close_attempts + 1
                assert(backing:close())
                return nil, "mock temp close failure"
            end,
        }
    elseif is_case_temp(close_failure_case, path) then
        unsafe_close_open_modes = unsafe_close_open_modes + 1
        return nil, "unsafe temp open mode"
    end
    return real_open(path, mode)
end
trigger_learning(close_failure_case, "close-must-fail")
io.open = real_open
assert(temp_close_attempts > 0, "close failure test must intercept temp close")
same(unsafe_close_open_modes, 0, "temp close path must use only r+ mode")
assert_storage_preserved(close_failure_case, "temp close failure")
ai_learned_translator.fini(close_failure_case.env)

local rename_failure_case = storage_case("rename failure")
local temp_rename_attempts = 0
os.rename = function(source, destination)
    if destination == rename_failure_case.path and source ~= destination then
        temp_rename_attempts = temp_rename_attempts + 1
        return nil, "mock atomic rename failure"
    end
    return real_rename(source, destination)
end
trigger_learning(rename_failure_case, "rename-must-fail")
os.rename = real_rename
assert(temp_rename_attempts > 0, "rename failure test must intercept the atomic replace")
assert_storage_preserved(rename_failure_case, "atomic rename failure")
ai_learned_translator.fini(rename_failure_case.env)

ai_learned_translator.fini(learn_env)
for _, event in ipairs({
    learn_context.select_notifier,
    learn_context.commit_notifier,
    learn_context.update_notifier,
}) do
    for _, slot in ipairs(event.slots) do
        assert(not slot.connected, "fini must disconnect every notifier connection")
    end
end

os.remove(invalid_path)
for index = #created_dirs, 1, -1 do
    local directory = created_dirs[index]
    os.remove(directory .. "/ai_weights.tsv")
    os.remove(directory .. "/ai_weights.tsv.tmp")
    assert_no_ai_temp_files(directory, "successful regression cleanup")
    run_command("/bin/rmdir " .. shell_quote(directory),
        "failed to remove temporary Rime user directory")
end
assert_file_absent(dollar_sentinel_path,
    "dollar command substitution must remain literal across every shell command")
assert_file_absent(backtick_sentinel_path,
    "backtick command substitution must remain literal across every shell command")
print("Rime AI regression OK")
