assert(type(auto_space_filter) == "function", "missing production auto_space_filter")
assert(type(select_character) == "function", "missing production select_character")

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
ShadowCandidate = function(source, kind, text, comment, ...)
    shadow_calls[#shadow_calls + 1] = {
        source = source,
        kind = kind,
        text = text,
        comment = comment,
        extra_count = select("#", ...),
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
UniquifiedCandidate = function(source, kind, text, comment, ...)
    uniquified_calls[#uniquified_calls + 1] = {
        source = source,
        kind = kind,
        text = text,
        comment = comment,
        extra_count = select("#", ...),
    }
    local mode = uniquified_mode
    if type(mode) == "table" then
        mode = table.remove(mode, 1) or "normal"
    end
    if mode == "throw" then
        error("injected UniquifiedCandidate failure")
    elseif mode == "nil" then
        return nil
    elseif mode == "noncandidate" then
        return "not a candidate"
    end
    return uniquified(kind, text or "", comment or "", source)
end

local yielded = nil
local yield_sink = nil
yield = function(value)
    assert(type(yield_sink) == "function", "missing active yield sink")
    return yield_sink(value)
end

local function stream(values, events)
    local state = {index = 0, values = values}
    return {
        iter = function()
            return function(current)
                assert(current == state, "translation iterator state must be forwarded")
                current.index = current.index + 1
                local value = current.values[current.index]
                if value ~= nil and events then
                    events[#events + 1] = "inner:" .. tostring(value.text)
                end
                return value
            end, state
        end,
    }
end

local function history_record(kind, text)
    return {type = kind, text = text}
end

local function run_auto_filter(history, candidates)
    local back_reads = 0
    local latest_text_reads = 0
    local commit_history = {}
    function commit_history:back()
        back_reads = back_reads + 1
        if type(history) == "function" then
            return history()
        end
        if type(history) == "string" then
            return history_record("phrase", history)
        end
        return history
    end
    function commit_history:latest_text()
        latest_text_reads = latest_text_reads + 1
        error("latest_text must not be consulted")
    end
    local env = {
        engine = {
            context = {
                commit_history = commit_history,
            },
        },
    }

    yielded = {}
    shadow_calls = {}
    uniquified_calls = {}
    local previous_sink = yield_sink
    yield_sink = function(value)
        yielded[#yielded + 1] = value
    end
    local ok, failure = pcall(auto_space_filter, stream(candidates), env)
    yield_sink = previous_sink
    assert(ok, "auto_space_filter must fail closed: " .. tostring(failure))
    same(latest_text_reads, 0, "commit history latest_text read count")
    same(back_reads, 1, "commit history back read count")
    return yielded
end

local function auto_filter_translation(history, candidates, events)
    local back_reads = 0
    local latest_text_reads = 0
    local commit_history = {}
    function commit_history:back()
        back_reads = back_reads + 1
        return history_record("phrase", history)
    end
    function commit_history:latest_text()
        latest_text_reads = latest_text_reads + 1
        error("latest_text must not be consulted")
    end
    local env = {
        engine = {
            context = {
                commit_history = commit_history,
            },
        },
    }

    shadow_calls = {}
    uniquified_calls = {}
    local coroutine_handle = coroutine.create(function()
        auto_space_filter(stream(candidates, events), env)
    end)
    local iterator_state = {}
    local translation = {
        iter = function()
            return function(current)
                assert(current == iterator_state,
                    "auto-filter iterator state must be forwarded")
                if coroutine.status(coroutine_handle) == "dead" then
                    return nil
                end
                local previous_sink = yield_sink
                yield_sink = function(value)
                    events[#events + 1] = "outer:" .. tostring(value.text)
                    return coroutine.yield(value)
                end
                local ok, value = coroutine.resume(coroutine_handle)
                yield_sink = previous_sink
                assert(ok, "lazy auto_space_filter failed: " .. tostring(value))
                return value
            end, iterator_state
        end,
    }
    local function verify_history_reads()
        same(latest_text_reads, 0, "lazy filter latest_text read count")
        same(back_reads, 1, "lazy filter history back read count")
    end
    return translation, verify_history_reads
end

local function run_spacing_pipeline(history, candidates)
    local events = {}
    local translation, verify_history_reads = auto_filter_translation(
        history, candidates, events
    )
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
            events[#events + 1] = "menu:" .. tostring(next_candidate.text)
        else
            local previous = menu[previous_index]
            if previous:get_dynamic_type() == "Uniquified" then
                previous:append(next_candidate)
            else
                previous = uniquified("uniquified", "", "", previous)
                previous:append(next_candidate)
                menu[previous_index] = previous
            end
            events[#events + 1] = "duplicate:" .. tostring(next_candidate.text)
        end
    end
    verify_history_reads()
    return menu, events
end

local function assert_pass_through(history, value, message)
    local output = run_auto_filter(history, {value})
    same(#output, 1, (message or "pass-through") .. " output count")
    assert(rawequal(output[1], value), (message or "candidate") ..
        " must pass through by identity")
    same(#uniquified_calls, 0,
        (message or "pass-through") .. " UniquifiedCandidate calls")
    same(#shadow_calls, 0, (message or "pass-through") .. " ShadowCandidate calls")
end

local function assert_spaced(history, text, message)
    local original = candidate("phrase", 0, 4, text, "comment", 1.2)
    local output = run_auto_filter(history, {original})
    same(#output, 1, message .. " output count")
    same(output[1].type, "auto_space", message .. " candidate type")
    same(output[1].text, " " .. text, message .. " candidate text")
    same(output[1]:get_dynamic_type(), "Uniquified",
        message .. " first spaced candidate dynamic type")
    assert(rawequal(output[1]:get_genuine(), original),
        message .. " wrapper must expose the original candidate")
    same(#output[1]:get_genuines(), 1, message .. " genuine count")
    same(#uniquified_calls, 1, message .. " UniquifiedCandidate calls")
    same(#shadow_calls, 0, message .. " ShadowCandidate calls")
end

local function assert_spaced_deduplication(count, message)
    local originals = {}
    for index = 1, count do
        originals[index] = candidate(
            "phrase", 0, 4, "Rime", "comment-" .. index, index
        )
    end

    local menu, events = run_spacing_pipeline("中文", originals)
    same(#menu, 1, message .. " menu count")
    local final = menu[1]
    same(final.type, "auto_space", message .. " logical type")
    same(final:get_dynamic_type(), "Uniquified", message .. " dynamic type")
    same(final.text, " Rime", message .. " display text")
    same(final.comment, "comment-1", message .. " first display comment")
    same(final.quality, count, message .. " maximum quality")
    same(#final.items, count, message .. " stored item count")
    assert(rawequal(final.items[1], originals[1]),
        message .. " first item must be the first ultimate candidate")

    for index = 2, count do
        local item = final.items[index]
        same(item:get_dynamic_type(), "Shadow",
            message .. " later item " .. index .. " dynamic type")
        assert(rawequal(item:get_genuine(), originals[index]),
            message .. " later item " .. index .. " must wrap its ultimate once")
    end

    local genuines = final:get_genuines()
    same(#genuines, count, message .. " genuine count")
    for index, original in ipairs(originals) do
        assert(rawequal(genuines[index], original),
            message .. " flat genuine order " .. index)
    end
    assert(rawequal(final:get_genuine(), originals[1]),
        message .. " first genuine")
    same(#uniquified_calls, 1, message .. " seed constructor count")
    same(#shadow_calls, count - 1, message .. " later ShadowCandidate count")
    return final, events, originals
end

local two_deduped, two_events = assert_spaced_deduplication(
    2, "two identical spaced candidates"
)
same(table.concat(two_events, "|"), table.concat({
    "inner:Rime",
    "outer: Rime",
    "menu: Rime",
    "inner:Rime",
    "outer: Rime",
    "duplicate: Rime",
}, "|"), "lazy filter/Menu event order")

local three_deduped = assert_spaced_deduplication(
    3, "three identical spaced candidates"
)

local interleaved_first = candidate("phrase", 0, 4, "Rime", "first", 3)
local interleaved_other = candidate("phrase", 0, 4, ",other", "other", 2)
local interleaved_second = candidate("phrase", 0, 4, "Rime", "second", 1)
local interleaved_third = candidate("phrase", 0, 4, "Rime", "third", 0)
local interleaved_menu = run_spacing_pipeline("中文", {
    interleaved_first,
    interleaved_other,
    interleaved_second,
    interleaved_third,
})
same(#interleaved_menu, 2, "interleaved duplicate menu count")
same(interleaved_menu[1].type, "auto_space", "interleaved duplicate logical type")
same(interleaved_menu[1]:get_dynamic_type(), "Uniquified",
    "interleaved duplicate dynamic type")
same(#interleaved_menu[1]:get_genuines(), 3,
    "interleaved duplicate genuine count")
local interleaved_genuines = interleaved_menu[1]:get_genuines()
for index, expected in ipairs({
    interleaved_first,
    interleaved_second,
    interleaved_third,
}) do
    assert(rawequal(interleaved_genuines[index], expected),
        "interleaved duplicate flat genuine order " .. index)
end
assert(rawequal(interleaved_menu[2], interleaved_other),
    "interleaved nonduplicate must preserve order and identity")

local distinct_first = candidate("phrase", 0, 4, "Rime", "first", 2)
local distinct_second = candidate("phrase", 0, 4, "Alpha", "second", 1)
local distinct_menu = run_spacing_pipeline("中文", {distinct_first, distinct_second})
same(#distinct_menu, 2, "distinct spaced text menu count")
for index, expected_text in ipairs({" Rime", " Alpha"}) do
    same(distinct_menu[index].type, "auto_space",
        "distinct spaced text " .. index .. " logical type")
    same(distinct_menu[index]:get_dynamic_type(), "Uniquified",
        "distinct spaced text " .. index .. " dynamic type")
    same(distinct_menu[index].text, expected_text,
        "distinct spaced text " .. index .. " display")
end
same(#uniquified_calls, 2, "distinct spaced text seed count")
same(#shadow_calls, 0, "distinct spaced text ShadowCandidate count")

local plain_first = candidate("phrase", 0, 4, "年", "first", 2)
local plain_second = candidate("phrase", 0, 4, "年", "second", 1)
local plain_menu = run_spacing_pipeline("2026", {plain_first, plain_second})
same(#plain_menu, 1, "non-spacing duplicate menu count")
same(plain_menu[1].type, "uniquified", "non-spacing builtin logical type")
same(plain_menu[1]:get_dynamic_type(), "Uniquified",
    "non-spacing builtin dynamic type")
local plain_genuines = plain_menu[1]:get_genuines()
assert(rawequal(plain_genuines[1], plain_first),
    "non-spacing first genuine")
assert(rawequal(plain_genuines[2], plain_second),
    "non-spacing second genuine")
same(#uniquified_calls, 0, "non-spacing auto seed count")
same(#shadow_calls, 0, "non-spacing auto ShadowCandidate count")

local natural_space = candidate("phrase", 0, 4, " Rime", "natural", 2)
local colliding_plain = candidate("phrase", 0, 4, "Rime", "plain", 1)
local collision_menu = run_spacing_pipeline("中文", {natural_space, colliding_plain})
same(#collision_menu, 2, "natural leading-space collision menu count")
assert(rawequal(collision_menu[1], natural_space),
    "natural leading-space candidate must remain first and unchanged")
assert(rawequal(collision_menu[2], colliding_plain),
    "colliding auto-space target must fail closed unspaced")
same(collision_menu[1].text, " Rime", "natural leading-space display")
same(collision_menu[2].text, "Rime", "collision fallback display")
same(#uniquified_calls, 0, "natural collision seed count")
same(#shadow_calls, 0, "natural collision ShadowCandidate count")

local terminal_get_genuine_calls = 0
local function fresh_terminal_alias()
    local value = candidate("phrase", 0, 4, "Rime", "", 1, "Simple")
    function value:get_genuine()
        terminal_get_genuine_calls = terminal_get_genuine_calls + 1
        return fresh_terminal_alias()
    end
    return value
end

local native_terminal = fresh_terminal_alias()
local native_terminal_output = run_auto_filter("中文", {native_terminal})[1]
same(terminal_get_genuine_calls, 0,
    "terminal dynamic type must resolve without calling get_genuine")
same(native_terminal_output.type, "auto_space", "native-like terminal must be wrapped")
assert(rawequal(native_terminal_output:get_genuine(), native_terminal),
    "native-like terminal must remain the UniquifiedCandidate source")

for _, terminal_type in ipairs({"Sentence", "Phrase", "Other"}) do
    local terminal_calls = 0
    local throwing_terminal = candidate(
        "phrase", 0, 4, "Rime", "", 1, terminal_type
    )
    function throwing_terminal:get_genuine()
        terminal_calls = terminal_calls + 1
        error("terminal get_genuine must not be called")
    end
    local throwing_terminal_output = run_auto_filter("中文", {throwing_terminal})[1]
    same(terminal_calls, 0,
        terminal_type .. " terminal must not probe get_genuine")
    same(throwing_terminal_output.type, "auto_space",
        terminal_type .. " terminal must be wrapped")
end

for _, boundary in ipairs({
    {history = "中文", text = "Rime", name = "Han to uppercase ASCII"},
    {history = "中文", text = "rime", name = "Han to lowercase ASCII"},
    {history = "Rime", text = "输入法", name = "uppercase ASCII to Han"},
    {history = "rime", text = "输入法", name = "lowercase ASCII to Han"},
}) do
    assert_spaced(boundary.history, boundary.text, boundary.name)
end

assert_spaced(history_record("custom_candidate", "中文"), "Rime",
    "arbitrary candidate history record type")

assert_pass_through(history_record("thru", "R"),
    candidate("phrase", 0, 4, "输入"), "direct ASCII thru history")
assert_pass_through(history_record("raw", "R"),
    candidate("phrase", 0, 4, "输入"), "raw ASCII to Han history")
assert_pass_through(history_record("raw", "中"),
    candidate("phrase", 0, 4, "Rime"), "raw Han to ASCII history")

for _, letter in ipairs({"A", "Z", "a", "z"}) do
    assert_spaced("中", letter, "Han to ASCII endpoint " .. letter)
    assert_spaced(letter, "中", "ASCII endpoint to Han " .. letter)
end

for _, nonletter in ipairs({"@", "[", "`", "{"}) do
    assert_pass_through("中", candidate("phrase", 0, 4, nonletter),
        "Han before adjacent ASCII nonletter " .. nonletter)
    assert_pass_through(nonletter, candidate("phrase", 0, 4, "中"),
        "adjacent ASCII nonletter before Han " .. nonletter)
end

local han_ranges = {
    {first = 0x3400, last = 0x4DBF, name = "CJK Extension A"},
    {first = 0x4E00, last = 0x9FFF, name = "CJK Unified Ideographs"},
    {first = 0xF900, last = 0xFAFF, name = "CJK Compatibility Ideographs"},
    {first = 0x20000, last = 0x2FA1F, name = "CJK supplementary extensions"},
    {first = 0x30000, last = 0x3347F, name = "CJK tertiary extensions"},
}

for _, range in ipairs(han_ranges) do
    for _, codepoint in ipairs({range.first, range.last}) do
        local han = utf8.char(codepoint)
        local label = string.format("%s U+%X", range.name, codepoint)
        assert_spaced(han, "A", label .. " Han to ASCII")
        assert_spaced("A", han, label .. " ASCII to Han")
    end
end

for _, range in ipairs(han_ranges) do
    for _, codepoint in ipairs({range.first - 1, range.last + 1}) do
        local outside = utf8.char(codepoint)
        local label = string.format("outside Han range U+%X", codepoint)
        assert_pass_through(outside, candidate("phrase", 0, 4, "A"),
            label .. " before ASCII")
        assert_pass_through("A", candidate("phrase", 0, 4, outside),
            label .. " after ASCII")
    end
end

for _, test in ipairs({
    {history = "第", text = "3次", name = "Han before digit"},
    {history = "2026", text = "年", name = "digit before Han"},
    {history = "中文", text = ",Rime", name = "punctuation boundary"},
    {history = "Rime", text = "。输入", name = "Han punctuation boundary"},
    {history = "中文", text = "😀Rime", name = "Emoji boundary"},
    {history = "中文 ", text = "Rime", name = "existing history whitespace"},
    {history = "中文", text = " Rime", name = "candidate-leading whitespace"},
    {history = "", text = "Rime", name = "empty history"},
    {history = "中文", text = "", name = "empty candidate text"},
    {history = string.char(0xFF), text = "Rime", name = "invalid history UTF-8"},
    {history = "中文", text = string.char(0xFF) .. "Rime", name = "invalid candidate UTF-8"},
}) do
    assert_pass_through(test.history, candidate("phrase", 0, 4, test.text), test.name)
end

assert_pass_through("中文", candidate("phrase", 1, 4, "Rime"),
    "non-initial segment candidate")
assert_pass_through("中文", candidate("auto_space", 0, 4, "Rime"),
    "already spaced candidate")

local first = candidate("phrase", 0, 4, "Alpha")
local second = candidate("phrase", 0, 4, ",punctuation")
local third = candidate("phrase", 0, 4, "beta")
local ordered = run_auto_filter("中", {first, second, third})
same(#ordered, 3, "filter must preserve candidate count")
assert(rawequal(ordered[1]:get_genuine(), first), "first candidate order")
assert(rawequal(ordered[2], second), "second candidate order and identity")
assert(rawequal(ordered[3]:get_genuine(), third), "third candidate order")
same(#uniquified_calls, 2, "only boundary candidates should be seeded")
same(#shadow_calls, 0, "distinct boundary candidates need no ShadowCandidate")
assert(rawequal(uniquified_calls[1].source, first),
    "first UniquifiedCandidate source order")
assert(rawequal(uniquified_calls[2].source, third),
    "second UniquifiedCandidate source order")

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
local call = uniquified_calls[1]
same(call.source, ultimate, "UniquifiedCandidate source must be ultimate genuine")
same(call.kind, "auto_space", "UniquifiedCandidate type")
same(call.text, " Rime Display", "spacing must use final display text")
same(call.comment, "final-comment", "spacing must use final display comment")
same(call.extra_count, 0, "UniquifiedCandidate must receive exactly four arguments")
assert(rawequal(output:get_genuine(), ultimate),
    "final UniquifiedCandidate must expose ultimate genuine")

local mutating_ultimate = candidate("phrase", 0, 4, "ultimate", "", 1)
local mutating_final = candidate(
    "uniquified", 0, 4, "Rime", "before-unwrapping", 1, "Uniquified"
)
function mutating_final:get_genuine()
    self.text = "changed-after-unwrapping"
    self.comment = "changed-after-unwrapping"
    return mutating_ultimate
end
local mutating_output = run_auto_filter("中文", {mutating_final})[1]
same(mutating_output.text, " Rime", "display text must be captured before unwrapping")
same(mutating_output.comment, "before-unwrapping",
    "display comment must be captured before unwrapping")

local han_genuine = candidate("phrase", 0, 4, "输入", "", 1)
local ascii_display = wrapper("Uniquified", "uniquified", "Rime", "", han_genuine)
local ascii_output = run_auto_filter("中文", {ascii_display})[1]
same(ascii_output.type, "auto_space", "boundary must use final ASCII display text")
assert(rawequal(ascii_output:get_genuine(), han_genuine),
    "ASCII display wrapper must retain Han genuine candidate")

local ascii_genuine = candidate("phrase", 0, 4, "Rime", "", 1)
local han_display = wrapper("Uniquified", "uniquified", "输入", "", ascii_genuine)
local han_output = run_auto_filter("Rime", {han_display})[1]
same(han_output.type, "auto_space", "boundary must use final Han display text")
assert(rawequal(han_output:get_genuine(), ascii_genuine),
    "Han display wrapper must retain ASCII genuine candidate")

local function wrapper_chain(depth)
    local base = candidate("phrase", 0, 4, "ultimate", "", 1)
    local current = base
    for index = 1, depth do
        current = wrapper("Shadow", "shadow_" .. index, "Rime", "", current)
    end
    return current, base
end

local sixteen, sixteen_ultimate = wrapper_chain(16)
local sixteen_output = run_auto_filter("中文", {sixteen})[1]
same(sixteen_output.type, "auto_space", "sixteen wrapper transitions must succeed")
assert(rawequal(sixteen_output:get_genuine(), sixteen_ultimate),
    "sixteen transitions must resolve the ultimate genuine candidate")

local seventeen = wrapper_chain(17)
assert_pass_through("中文", seventeen,
    "seventeen wrapper transitions")

local cycle_a = candidate("uniquified", 0, 4, "Rime", "", 0, "Uniquified")
local cycle_b = candidate("simplified", 0, 4, "Rime", "", 0, "Shadow")
function cycle_a:get_genuine()
    return cycle_b
end
function cycle_b:get_genuine()
    return cycle_a
end
assert_pass_through("中文", cycle_a, "genuine cycle")

local throwing_genuine = candidate("uniquified", 0, 4, "Rime", "", 0, "Uniquified")
function throwing_genuine:get_genuine()
    error("injected get_genuine failure")
end
assert_pass_through("中文", throwing_genuine, "throwing genuine")

local nil_genuine = candidate("uniquified", 0, 4, "Rime", "", 0, "Uniquified")
function nil_genuine:get_genuine()
    return nil
end
assert_pass_through("中文", nil_genuine, "nil genuine")

local non_candidate_genuine = candidate(
    "uniquified", 0, 4, "Rime", "", 0, "Uniquified"
)
function non_candidate_genuine:get_genuine()
    return {}
end
assert_pass_through("中文", non_candidate_genuine, "non-candidate genuine")

local mismatched_genuine = candidate("phrase", 1, 4, "ultimate")
local mismatched_final = wrapper(
    "Uniquified", "uniquified", "Rime", "", mismatched_genuine, 0, 4
)
assert_pass_through("中文", mismatched_final, "genuine span mismatch")

local missing_span_genuine = candidate("phrase", nil, 4, "ultimate")
local missing_span_final = wrapper(
    "Uniquified", "uniquified", "Rime", "", missing_span_genuine, 0, 4
)
assert_pass_through("中文", missing_span_final, "missing genuine span")

local mismatched_end_genuine = candidate("phrase", 0, 5, "ultimate")
local mismatched_end_final = wrapper(
    "Uniquified", "uniquified", "Rime", "", mismatched_end_genuine, 0, 4
)
assert_pass_through("中文", mismatched_end_final, "genuine end mismatch")

local missing_end_genuine = candidate("phrase", 0, nil, "ultimate")
local missing_end_final = wrapper(
    "Uniquified", "uniquified", "Rime", "", missing_end_genuine, 0, 4
)
assert_pass_through("中文", missing_end_final, "missing genuine end")

local commented_genuine = candidate("phrase", 0, 4, "ultimate", "genuine-comment")
local empty_comment_final = wrapper(
    "Uniquified", "uniquified", "Rime", "", commented_genuine
)
assert_pass_through("中文", empty_comment_final, "unsafe empty final comment")

local throwing_dynamic_type = candidate("phrase", 0, 4, "Rime")
function throwing_dynamic_type:get_dynamic_type()
    error("injected get_dynamic_type failure")
end
assert_pass_through("中文", throwing_dynamic_type, "throwing dynamic type")

local nil_dynamic_type = candidate("phrase", 0, 4, "Rime")
function nil_dynamic_type:get_dynamic_type()
    return nil
end
assert_pass_through("中文", nil_dynamic_type, "nil dynamic type")

local unknown_dynamic_type = candidate("phrase", 0, 4, "Rime")
function unknown_dynamic_type:get_dynamic_type()
    return "FutureCandidate"
end
assert_pass_through("中文", unknown_dynamic_type, "unknown dynamic type")

local non_string_dynamic_type = candidate("phrase", 0, 4, "Rime")
function non_string_dynamic_type:get_dynamic_type()
    return 42
end
assert_pass_through("中文", non_string_dynamic_type, "non-string dynamic type")

local type_access_fault = setmetatable({text = "R"}, {
    __index = function(_, key)
        if key == "type" then
            error("injected history type access failure")
        end
    end,
})
assert_pass_through(type_access_fault, candidate("phrase", 0, 4, "中"),
    "history type access failure")

local text_access_fault = setmetatable({type = "phrase"}, {
    __index = function(_, key)
        if key == "text" then
            error("injected history text access failure")
        end
    end,
})
assert_pass_through(text_access_fault, candidate("phrase", 0, 4, "中"),
    "history text access failure")

for _, invalid_history in ipairs({
    {name = "nil record", record = false},
    {name = "missing record type", record = {text = "R"}},
    {name = "empty record type", record = history_record("", "R")},
    {name = "non-string record type", record = history_record(42, "R")},
    {name = "missing record text", record = {type = "phrase"}},
    {name = "empty record text", record = history_record("phrase", "")},
    {name = "non-string record text", record = history_record("phrase", 42)},
}) do
    local record = invalid_history.record
    if record == false then
        record = nil
    end
    assert_pass_through(record, candidate("phrase", 0, 4, "中"),
        invalid_history.name)
end

for _, mode in ipairs({"throw", "nil", "noncandidate"}) do
    local source = candidate("phrase", 0, 4, "Rime")
    uniquified_mode = mode
    local output = run_auto_filter("中文", {source})
    same(#output, 1, mode .. " UniquifiedCandidate output count")
    assert(rawequal(output[1], source),
        mode .. " UniquifiedCandidate must pass through original")
    same(#uniquified_calls, 1, mode .. " UniquifiedCandidate call count")
    same(#shadow_calls, 0, mode .. " seed failure ShadowCandidate call count")
end
uniquified_mode = "normal"

for _, mode in ipairs({"throw", "nil", "noncandidate"}) do
    local first_source = candidate("phrase", 0, 4, "Rime", "first")
    local later_source = candidate("phrase", 0, 4, "Rime", "later")
    uniquified_mode = {mode, "normal"}
    local output = run_auto_filter("中文", {first_source, later_source})
    same(#output, 2, mode .. " failed seed duplicate output count")
    assert(rawequal(output[1], first_source),
        mode .. " failed seed first original")
    assert(rawequal(output[2], later_source),
        mode .. " failed seed must block later duplicate spacing")
    same(#uniquified_calls, 1,
        mode .. " failed seed must not retry the target text")
    same(#shadow_calls, 0,
        mode .. " failed seed duplicate ShadowCandidate count")
end
uniquified_mode = "normal"

for _, mode in ipairs({"throw", "nil", "noncandidate"}) do
    local first_source = candidate("phrase", 0, 4, "Rime", "first")
    local later_source = candidate("phrase", 0, 4, "Rime", "later")
    shadow_mode = mode
    local output = run_auto_filter("中文", {first_source, later_source})
    same(#output, 2, mode .. " ShadowCandidate output count")
    same(output[1]:get_dynamic_type(), "Uniquified",
        mode .. " ShadowCandidate must retain the first seed")
    assert(rawequal(output[1]:get_genuine(), first_source),
        mode .. " ShadowCandidate first seed genuine")
    assert(rawequal(output[2], later_source),
        mode .. " ShadowCandidate must pass through later original")
    same(#uniquified_calls, 1, mode .. " ShadowCandidate seed count")
    same(#shadow_calls, 1, mode .. " ShadowCandidate call count")
end
shadow_mode = "normal"

local history_failure_source = candidate("phrase", 0, 4, "Rime")
assert_pass_through(function()
    error("injected commit history failure")
end, history_failure_source, "throwing commit history back")

local function processor_candidate(kind, display, genuine_behavior)
    local value = {
        type = kind,
        text = display,
    }
    function value:get_genuine()
        if genuine_behavior == "throw" then
            error("injected selected get_genuine failure")
        elseif genuine_behavior == "nil" then
            return nil
        elseif genuine_behavior == "noncandidate" then
            return "not a candidate"
        end
        return genuine_behavior
    end
    return value
end

local function processor_segment(selected)
    local value = {}
    function value:get_selected_candidate()
        return selected
    end
    return value
end

local function processor_segmentation(segments)
    local value = {
        size = #segments,
        requested_indices = {},
    }
    function value:empty()
        return self.size == 0
    end
    function value:get_at(index)
        self.requested_indices[#self.requested_indices + 1] = index
        return segments[index + 1]
    end
    return value
end

local function processor_composition(segmentation)
    local value = {}
    function value:empty()
        return segmentation:empty()
    end
    function value:toSegmentation()
        return segmentation
    end
    return value
end

local function composition_with(first_selected, second_selected)
    local segments = {processor_segment(first_selected)}
    if second_selected ~= nil then
        segments[#segments + 1] = processor_segment(second_selected)
    end
    local segmentation = processor_segmentation(segments)
    return processor_composition(segmentation), segmentation
end

local function processor_key(representation)
    local value = {}
    function value:repr()
        return representation
    end
    return value
end

local function run_select_character(representation, commit_text, composition)
    local commits = {}
    local clear_count = 0
    local config = {}
    function config:get_string(path)
        if path == "key_binder/select_first_character" then
            return "Control+j"
        elseif path == "key_binder/select_last_character" then
            return "Control+l"
        end
        return nil
    end

    local context = {composition = composition}
    function context:get_commit_text()
        return commit_text
    end
    function context:clear()
        clear_count = clear_count + 1
    end

    local engine = {
        context = context,
        schema = {config = config},
    }
    function engine:commit_text(text)
        commits[#commits + 1] = text
    end

    local ok, result = pcall(
        select_character,
        processor_key(representation),
        {engine = engine}
    )
    assert(ok, "select_character must fail closed: " .. tostring(result))
    return {
        result = result,
        commits = commits,
        clear_count = clear_count,
    }
end

local function assert_accepted(outcome, expected, message)
    same(outcome.result, 1, message .. " return value")
    same(#outcome.commits, 1, message .. " commit count")
    same(outcome.commits[1], expected, message)
    same(outcome.clear_count, 1, message .. " clear count")
end

local genuine_for_selection = candidate(
    "phrase", 0, 4, "与显示文本刻意不同", "", 1
)

for _, case in ipairs({
    {key = "Control+j", expected = " R", name = "deduplicated Ctrl+J"},
    {key = "Control+l", expected = " e", name = "deduplicated Ctrl+L"},
}) do
    local composition, segmentation = composition_with(two_deduped)
    local outcome = run_select_character(case.key, " Rime", composition)
    assert_accepted(outcome, case.expected,
        case.name .. " must consume the simulated final-uniquifier result")
    same(#segmentation.requested_indices, 1,
        case.name .. " segmentation lookup count")
    same(segmentation.requested_indices[1], 0,
        case.name .. " first segment index")
end

for _, case in ipairs({
    {key = "Control+j", expected = " R", name = "Ctrl+J must preserve automatic prefix"},
    {key = "Control+l", expected = " e", name = "Ctrl+L must preserve automatic prefix"},
}) do
    local selected = processor_candidate(
        "auto_space", " Rime", genuine_for_selection
    )
    local composition, segmentation = composition_with(selected)
    local outcome = run_select_character(case.key, " Rime", composition)
    assert_accepted(outcome, case.expected, case.name)
    same(#segmentation.requested_indices, 1,
        case.name .. " segmentation lookup count")
    same(segmentation.requested_indices[1], 0,
        case.name .. " must inspect zero-based first segment")
end

for _, case in ipairs({
    {key = "Control+j", expected = " R", name = "multi-segment Ctrl+J"},
    {key = "Control+l", expected = " 法", name = "multi-segment Ctrl+L"},
}) do
    local first_selected = processor_candidate(
        "auto_space", " Rime", genuine_for_selection
    )
    local active_second_selected = processor_candidate(
        "phrase", "输入法", genuine_for_selection
    )
    local composition, segmentation = composition_with(
        first_selected, active_second_selected
    )
    local outcome = run_select_character(
        case.key, " Rime输入法", composition
    )
    assert_accepted(outcome, case.expected,
        case.name .. " must preserve automatic prefix")
    same(#segmentation.requested_indices, 1,
        case.name .. " segmentation lookup count")
    same(segmentation.requested_indices[1], 0,
        case.name .. " must inspect first segment when the second is active")
end

local function assert_old_selection(
        message, commit_text, composition_factory, first_expected, last_expected)
    for _, case in ipairs({
        {key = "Control+j", expected = first_expected, label = "Ctrl+J"},
        {key = "Control+l", expected = last_expected, label = "Ctrl+L"},
    }) do
        local outcome = run_select_character(
            case.key, commit_text, composition_factory()
        )
        assert_accepted(outcome, case.expected,
            message .. " " .. case.label .. " must retain old behavior")
    end
end

local function valid_selection_composition(kind, display, genuine_behavior)
    local selected = processor_candidate(kind, display, genuine_behavior)
    return composition_with(selected)
end

assert_old_selection(
    "non-auto-space candidate", " Rime",
    function()
        return valid_selection_composition(
            "phrase", " Rime", genuine_for_selection
        )
    end,
    " ", "e"
)

assert_old_selection(
    "zero-leading-space display", "Rime",
    function()
        return valid_selection_composition(
            "auto_space", "Rime", genuine_for_selection
        )
    end,
    "R", "e"
)

assert_old_selection(
    "multiple-leading-space display", "  Rime",
    function()
        return valid_selection_composition(
            "auto_space", "  Rime", genuine_for_selection
        )
    end,
    " ", "e"
)

assert_old_selection(
    "full commit without exact selected display prefix", " X Rime",
    function()
        return valid_selection_composition(
            "auto_space", " Rime", genuine_for_selection
        )
    end,
    " ", "e"
)

for _, failure in ipairs({
    {
        name = "missing composition",
        composition = function()
            return nil
        end,
    },
    {
        name = "missing composition empty API",
        composition = function()
            local segmentation = processor_segmentation({
                processor_segment(processor_candidate(
                    "auto_space", " Rime", genuine_for_selection
                )),
            })
            return {
                toSegmentation = function()
                    return segmentation
                end,
            }
        end,
    },
    {
        name = "throwing composition empty API",
        composition = function()
            local composition = select(1, valid_selection_composition(
                "auto_space", " Rime", genuine_for_selection
            ))
            function composition:empty()
                error("injected composition empty failure")
            end
            return composition
        end,
    },
    {
        name = "indeterminate composition empty state",
        composition = function()
            local composition = select(1, valid_selection_composition(
                "auto_space", " Rime", genuine_for_selection
            ))
            function composition:empty()
                return nil
            end
            return composition
        end,
    },
    {
        name = "empty composition",
        composition = function()
            local composition = select(1, valid_selection_composition(
                "auto_space", " Rime", genuine_for_selection
            ))
            function composition:empty()
                return true
            end
            return composition
        end,
    },
    {
        name = "missing toSegmentation API",
        composition = function()
            return {empty = function() return false end}
        end,
    },
    {
        name = "throwing toSegmentation API",
        composition = function()
            return {
                empty = function() return false end,
                toSegmentation = function()
                    error("injected toSegmentation failure")
                end,
            }
        end,
    },
    {
        name = "nil segmentation",
        composition = function()
            return {
                empty = function() return false end,
                toSegmentation = function() return nil end,
            }
        end,
    },
    {
        name = "missing segmentation empty API",
        composition = function()
            local segment = processor_segment(processor_candidate(
                "auto_space", " Rime", genuine_for_selection
            ))
            local segmentation = {
                size = 1,
                get_at = function(_, index)
                    return index == 0 and segment or nil
                end,
            }
            return {
                empty = function() return false end,
                toSegmentation = function() return segmentation end,
            }
        end,
    },
    {
        name = "throwing segmentation empty API",
        composition = function()
            local segmentation = processor_segmentation({
                processor_segment(processor_candidate(
                    "auto_space", " Rime", genuine_for_selection
                )),
            })
            function segmentation:empty()
                error("injected segmentation empty failure")
            end
            return {
                empty = function() return false end,
                toSegmentation = function() return segmentation end,
            }
        end,
    },
    {
        name = "indeterminate segmentation empty state",
        composition = function()
            local segmentation = processor_segmentation({
                processor_segment(processor_candidate(
                    "auto_space", " Rime", genuine_for_selection
                )),
            })
            function segmentation:empty()
                return nil
            end
            return {
                empty = function() return false end,
                toSegmentation = function() return segmentation end,
            }
        end,
    },
    {
        name = "empty segmentation",
        composition = function()
            local segmentation = processor_segmentation({})
            return {
                empty = function() return false end,
                toSegmentation = function() return segmentation end,
            }
        end,
    },
    {
        name = "missing segmentation get_at API",
        composition = function()
            local segmentation = {
                size = 1,
                empty = function() return false end,
            }
            return processor_composition(segmentation)
        end,
    },
    {
        name = "throwing segmentation get_at API",
        composition = function()
            local segmentation = {
                size = 1,
                empty = function() return false end,
                get_at = function()
                    error("injected get_at failure")
                end,
            }
            return processor_composition(segmentation)
        end,
    },
    {
        name = "absent first segment",
        composition = function()
            local segmentation = {
                size = 1,
                empty = function() return false end,
                get_at = function() return nil end,
            }
            return processor_composition(segmentation)
        end,
    },
    {
        name = "missing segment selected-candidate API",
        composition = function()
            return processor_composition(processor_segmentation({{}}))
        end,
    },
    {
        name = "throwing segment selected-candidate API",
        composition = function()
            local segment = {}
            function segment:get_selected_candidate()
                error("injected selected-candidate failure")
            end
            return processor_composition(processor_segmentation({segment}))
        end,
    },
    {
        name = "first segment without selected candidate",
        composition = function()
            return processor_composition(processor_segmentation({
                processor_segment(nil),
            }))
        end,
    },
    {
        name = "selected get_genuine returns nil",
        composition = function()
            return valid_selection_composition(
                "auto_space", " Rime", "nil"
            )
        end,
    },
    {
        name = "selected get_genuine throws",
        composition = function()
            return valid_selection_composition(
                "auto_space", " Rime", "throw"
            )
        end,
    },
    {
        name = "selected get_genuine returns noncandidate",
        composition = function()
            return valid_selection_composition(
                "auto_space", " Rime", "noncandidate"
            )
        end,
    },
    {
        name = "selected type property throws",
        composition = function()
            local selected = {
                text = " Rime",
                get_genuine = function() return genuine_for_selection end,
            }
            setmetatable(selected, {
                __index = function(_, key)
                    if key == "type" then
                        error("injected selected type failure")
                    end
                end,
            })
            return composition_with(selected)
        end,
    },
    {
        name = "selected text property throws",
        composition = function()
            local selected = {
                type = "auto_space",
                get_genuine = function() return genuine_for_selection end,
            }
            setmetatable(selected, {
                __index = function(_, key)
                    if key == "text" then
                        error("injected selected text failure")
                    end
                end,
            })
            return composition_with(selected)
        end,
    },
}) do
    assert_old_selection(
        failure.name, " Rime", failure.composition, " ", "e"
    )
end

local unrelated_composition = select(1, valid_selection_composition(
    "auto_space", " Rime", genuine_for_selection
))
local unrelated = run_select_character(
    "Control+x", " Rime", unrelated_composition
)
same(unrelated.result, 2, "unrelated key return value")
same(#unrelated.commits, 0, "unrelated key commit count")
same(unrelated.clear_count, 0, "unrelated key clear count")

local unrelated_reads = {
    key_repr = 0,
    commit_text = 0,
    composition = 0,
    proof_api = 0,
    commit = 0,
    clear = 0,
}
local guarded_composition = {}
function guarded_composition:empty()
    unrelated_reads.proof_api = unrelated_reads.proof_api + 1
    return true
end
function guarded_composition:toSegmentation()
    unrelated_reads.proof_api = unrelated_reads.proof_api + 1
    error("unrelated key must not traverse segmentation")
end

local guarded_context = {}
function guarded_context:get_commit_text()
    unrelated_reads.commit_text = unrelated_reads.commit_text + 1
    return " Rime"
end
function guarded_context:clear()
    unrelated_reads.clear = unrelated_reads.clear + 1
end
setmetatable(guarded_context, {
    __index = function(_, key)
        if key == "composition" then
            unrelated_reads.composition = unrelated_reads.composition + 1
            return guarded_composition
        end
    end,
})

local guarded_config = {}
function guarded_config:get_string(path)
    if path == "key_binder/select_first_character" then
        return "Control+j"
    elseif path == "key_binder/select_last_character" then
        return "Control+l"
    end
    return nil
end

local guarded_engine = {
    context = guarded_context,
    schema = {config = guarded_config},
}
function guarded_engine:commit_text()
    unrelated_reads.commit = unrelated_reads.commit + 1
end

local guarded_key = {}
function guarded_key:repr()
    unrelated_reads.key_repr = unrelated_reads.key_repr + 1
    return "Control+x"
end

local guarded_ok, guarded_result = pcall(
    select_character,
    guarded_key,
    {engine = guarded_engine}
)
assert(guarded_ok,
    "unrelated key must not fail: " .. tostring(guarded_result))
same(guarded_result, 2, "guarded unrelated key return value")
same(unrelated_reads.commit, 0, "guarded unrelated key commit count")
same(unrelated_reads.clear, 0, "guarded unrelated key clear count")
same(unrelated_reads.key_repr, 1, "unrelated key repr read count")
same(unrelated_reads.commit_text, 0, "unrelated key commit-text read count")
same(unrelated_reads.composition, 0, "unrelated key composition read count")
same(unrelated_reads.proof_api, 0, "unrelated key proof-API call count")

print("Rime auto-space regression OK")
