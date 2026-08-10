#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
schema="$repo_root/rime/double_pinyin_flypy.schema.yaml"
others_dict="$repo_root/rime/cn_dicts/others.dict.yaml"
melt_eng_dict="$repo_root/rime/melt_eng.dict.yaml"
squirrel="$repo_root/rime/darwin/squirrel.custom.yaml"
ai_harness="$repo_root/tests/config/rime-ai-regression.lua"
ai_readme="$repo_root/rime/squirrel-ai/README.md"

reject_match() {
  local pattern="$1" file="$2" message="$3" rg_status=0

  rg -- "$pattern" "$file" >/dev/null || rg_status=$?
  case "$rg_status" in
    0)
      print -u2 -- "$message"
      exit 1
      ;;
    1) ;;
    *)
      print -u2 -- "failed to inspect $file with rg (exit $rg_status)"
      exit 1
      ;;
  esac
}

for config_file in "$schema" "$others_dict" "$melt_eng_dict" "$squirrel" "$ai_harness"; do
  [[ -f "$config_file" ]] || {
    print -u2 "expected Rime config at $config_file"
    exit 1
  }
done

[[ -f "$ai_readme" ]] || {
  print -u2 "expected Squirrel AI README at $ai_readme"
  exit 1
}

reject_match '^[[:space:]]*-[[:space:]]*lua_filter@reduce_english_filter([[:space:]]|$)' "$schema" \
  "expected active lua_filter@reduce_english_filter to be removed"
reject_match '^[[:space:]]*-[[:space:]]*reverse_lookup_translator@xklbdz([[:space:]]|$)' "$schema" \
  "expected active reverse_lookup_translator@xklbdz to be removed"
reject_match '^[[:space:]]*reverse_lookup:[[:space:]]*"\^u\[a-z\]\+\$"[[:space:]]*(#.*)?$' "$schema" \
  'expected active reverse_lookup: "^u[a-z]+$" xklbdz recognizer pattern to be removed'

rg '^[[:space:]]*unicode:[[:space:]]*"\^U\[0-9A-Fa-f\]\+\$"[[:space:]]*(#.*)?$' "$schema" >/dev/null || {
  print -u2 'expected active unicode: "^U[0-9A-Fa-f]+$" recognizer pattern'
  exit 1
}

for reading in 'yi qi dang qian' 'yi ji dang qian'; do
  reject_match "^一骑当千 +${reading}([[:space:]]|$)" "$others_dict" \
    "expected malformed space-separated 一骑当千 $reading entry to be removed"
  rg -- $'^一骑当千\t'"$reading"$'(\t[^\t]+)?$' "$others_dict" >/dev/null || {
    print -u2 "expected 一骑当千 $reading to use a TAB separator"
    exit 1
  }
done

reject_match '^[[:space:]]*-[[:space:]]*en_dicts/cn_en([[:space:]]|$)' "$melt_eng_dict" \
  "expected active en_dicts/cn_en import to be removed"

reject_match '(?i)(api[_-]?key|authorization[[:space:]]*:[[:space:]]*bearer|sk-[[:alnum:]_-]{8,})' "$squirrel" \
  "expected Squirrel AI config to contain no API key or bearer secret"

ruby -ryaml - "$schema" "$squirrel" <<'RUBY'
schema_path, squirrel_path = ARGV
schema = YAML.load_file(schema_path)
engine = schema.fetch("engine")
translators = engine.fetch("translators")
filters = engine.fetch("filters")

script_index = translators.index("script_translator")
unless script_index && translators[script_index + 1] == "lua_translator@ai_learned_translator"
  abort "expected lua_translator@ai_learned_translator immediately after script_translator"
end

uniquifier_index = filters.index("uniquifier")
unless uniquifier_index&.positive? && filters[uniquifier_index - 1] == "lua_filter@ai_candidate_filter"
  abort "expected lua_filter@ai_candidate_filter immediately before uniquifier"
end

patch = YAML.load_file(squirrel_path).fetch("patch")
endpoint = patch.fetch("ai/endpoint")
model = patch.fetch("ai/model")
unless endpoint.is_a?(String) && model.is_a?(String) && endpoint.empty? == model.empty?
  abort "expected ai/endpoint and ai/model to be strings configured together"
end
unless endpoint.empty?
  require "uri"
  begin
    uri = URI.parse(endpoint)
  rescue URI::InvalidURIError
    abort "expected ai/endpoint to be a valid HTTPS URL"
  end
  unless uri.is_a?(URI::HTTPS) && uri.host && !uri.host.empty? &&
      uri.user.nil? && uri.password.nil? && model == model.strip && !model.empty?
    abort "expected a credential-free HTTPS ai/endpoint and non-empty trimmed ai/model"
  end
end
unless patch["ai/enabled"] == true
  abort "expected ai/enabled to default to true in squirrel.custom.yaml patch"
end
instructions = patch.fetch("ai/instructions")
unless instructions.is_a?(String) && !instructions.empty? &&
    instructions == instructions.strip && instructions.length <= 4_096
  abort "expected non-empty normalized ai/instructions within the runtime limit"
end

[
  "简体中文输入法",
  "前后鼻音混淆",
  "按键顺序颠倒",
  "相邻按键误触",
  "漏键和多键",
  "优先选择已有候选",
  "不翻译、不扩写、不润色",
  "无法确定时",
].each do |requirement|
  abort "expected ai/instructions correction rule: #{requirement}" unless
    instructions.include?(requirement)
end
RUBY

ruby - "$ai_readme" <<'RUBY'
readme = File.read(ARGV.fetch(0))
recovery = readme[/If `ditto`.*?\n## Configure Rime and Keychain/m] or
  abort "expected self-contained Squirrel rollback section"

[
  'restored_executable="$installed_app/Contents/MacOS/Squirrel"',
  'console_user="$(/usr/bin/stat -f%Su /dev/console)"',
  '/usr/bin/sudo -u "$console_user" /usr/bin/killall Squirrel >/dev/null 2>&1 || true',
  '"$restored_executable" --register-input-source',
  '/usr/bin/sudo -u "$console_user" "$restored_executable" --enable-input-source',
  '/usr/bin/sudo -u "$console_user" "$restored_executable" --select-input-source',
].each do |command|
  abort "expected self-contained rollback command: #{command}" unless recovery.include?(command)
end

abort "rollback must not depend on a temporary source checkout" if
  recovery.include?("squirrel_checkout") || recovery.include?("scripts/postinstall")

summary = readme[/## Roll back or upgrade.*\z/m] or abort "expected rollback summary"
abort "rollback summary must describe restored-bundle registration" unless
  %w[--register-input-source --enable-input-source --select-input-source].all? { |flag| summary.include?(flag) }
abort "rollback summary must not depend on postinstall" if summary.include?("postinstall")

%w[ai/enabled ai/instructions 0600].each do |runtime_contract|
  abort "expected README runtime contract: #{runtime_contract}" unless readme.include?(runtime_contract)
end
abort "README must include a multiline ai/instructions |- YAML example" unless
  readme.match?(/ai\/instructions:\s*\|-\s*\n[ \t]+\S[^\n]*\n[ \t]+\S[^\n]*(?:\n|\z)/)

[
  'Changing `ai/enabled` or `ai/instructions` requires only `--reload`; it does not require rebuilding Squirrel.',
  '`ai/instructions` cannot override the mandatory request/response protocol.',
  'Changing the mandatory Swift protocol behavior still requires regenerating the patches and rebuilding Squirrel.',
].each do |runtime_statement|
  abort "expected explicit README statement: #{runtime_statement}" unless readme.include?(runtime_statement)
end
RUBY

(
umask 022
ruby -rfiddle - "$repo_root/rime/rime.lua" "$ai_harness" <<'RUBY'
core = Fiddle::Handle.new(
  "/Library/Input Methods/Squirrel.app/Contents/Frameworks/librime.1.dylib",
  Fiddle::RTLD_GLOBAL | Fiddle::RTLD_LAZY
)
lua = Fiddle::Handle.new(
  "/Library/Input Methods/Squirrel.app/Contents/Frameworks/rime-plugins/librime-lua.dylib",
  Fiddle::RTLD_LAZY
)

function = ->(name, arguments, result) {
  Fiddle::Function.new(lua[name], arguments, result)
}
new_state = function.call("luaL_newstate", [], Fiddle::TYPE_VOIDP)
open_libs = function.call("luaL_openlibs", [Fiddle::TYPE_VOIDP], Fiddle::TYPE_VOID)
load_file = function.call(
  "luaL_loadfilex",
  [Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP, Fiddle::TYPE_VOIDP],
  Fiddle::TYPE_INT
)
protected_call = function.call(
  "lua_pcallk",
  [Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT, Fiddle::TYPE_INT, Fiddle::TYPE_INT,
   Fiddle::TYPE_INTPTR_T, Fiddle::TYPE_VOIDP],
  Fiddle::TYPE_INT
)
to_string = function.call(
  "lua_tolstring",
  [Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT, Fiddle::TYPE_VOIDP],
  Fiddle::TYPE_VOIDP
)
close = function.call("lua_close", [Fiddle::TYPE_VOIDP], Fiddle::TYPE_VOID)

state = new_state.call
abort "failed to create bundled Lua state" if state.to_i.zero?

begin
  open_libs.call(state)
  ARGV.each do |path|
    status = load_file.call(state, path, 0)
    if status.zero?
      status = protected_call.call(state, 0, 0, 0, 0, 0)
    end
    next if status.zero?

    pointer = to_string.call(state, -1, 0)
    message = pointer.to_i.zero? ? "unknown Lua error" : Fiddle::Pointer.new(pointer).to_s
    abort "#{path}: #{message}"
  end
ensure
  close.call(state)
end
RUBY
)
