assert(type(ai_candidate_filter) == "function", "missing production ai_candidate_filter")
assert(type(ai_learned_translator) == "table", "missing production ai_learned_translator")
assert(type(ai_learned_translator.init) == "function", "missing ai_learned_translator.init")
assert(type(ai_learned_translator.func) == "function", "missing ai_learned_translator.func")
assert(type(ai_learned_translator.fini) == "function", "missing ai_learned_translator.fini")

local function same(actual, expected, message)
    assert(actual == expected, (message or "values differ") ..
        ": expected " .. tostring(expected) .. ", got " .. tostring(actual))
end

local function candidate(kind, start_pos, end_pos, text)
    local value = {
        type = kind,
        start = start_pos,
        _end = end_pos,
        text = text,
        comment = "",
        quality = 0,
    }
    function value:get_genuine()
        return self
    end
    return value
end

Candidate = function(kind, start_pos, end_pos, text, comment)
    local value = candidate(kind, start_pos, end_pos, text)
    value.comment = comment
    return value
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

local function env(input, segment, schema_id, initial_quality)
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
                },
            },
        },
        name_space = "ai_learned_translator",
    }, ctx
end

local no_segment = {}
local function run_filter(values, properties, input, segment)
    local active_segment = segment
    if segment == nil then
        active_segment = {start = 0, _end = 4, status = "selected"}
    elseif segment == no_segment then
        active_segment = nil
    end
    local filter_env, ctx = env(input or "code", active_segment)
    ctx.properties = properties or {}
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

local temp_dir = os.tmpname()
os.remove(temp_dir)
local mkdir_ok = os.execute("mkdir " .. string.format("%q", temp_dir))
assert(mkdir_ok == true or mkdir_ok == 0, "failed to create temporary Rime user directory")
local weights_path = temp_dir .. "/ai_weights.tsv"
rime_api = {
    get_user_data_dir = function()
        return temp_dir
    end,
}

local function read_file(path)
    local file = io.open(path, "r")
    if not file then
        return nil
    end
    local contents = file:read("*a")
    file:close()
    return contents
end

local function write_file(path, contents)
    local file = assert(io.open(path, "w"))
    assert(file:write(contents))
    assert(file:close())
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
local learn_env, learn_context = env("xxcodeyy", selected_segment, "test_schema")
ai_learned_translator.init(learn_env)
same(learn_context.select_notifier.slots[1].group, 0, "select notifier group")
same(learn_context.commit_notifier.slots[1].group, 0, "commit notifier group")
same(learn_context.update_notifier.slots[1].group, 0, "update notifier group")

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
same(read_file(weights_path), nil, "selection must not persist before commit")
learn_context.input = "xxcodeyy"
learn_context.update_notifier:emit(learn_context)
learn_context.commit_notifier:emit(learn_context)
local learned = read_file(weights_path)
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

for _, invalid_quality in ipairs({false, "not-a-number"}) do
    local safe_quality_env = env("code", {start = 0, _end = 4}, "test_schema", invalid_quality)
    yielded = {}
    ai_learned_translator.func("code", {start = 0, _end = 4}, safe_quality_env)
    assert(#yielded > 0 and yielded[1].quality > 0,
        "missing or non-numeric initial quality must use a safe baseline")
end

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

os.remove(weights_path)
local rmdir_ok = os.execute("rmdir " .. string.format("%q", temp_dir))
assert(rmdir_ok == true or rmdir_ok == 0, "failed to remove temporary Rime user directory")
print("Rime AI regression OK")
