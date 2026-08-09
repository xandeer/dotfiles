#!/bin/zsh

set -eu

[[ $# -eq 1 ]] || {
  print -u2 -- "usage: $0 /path/to/Squirrel"
  exit 2
}

checkout="${1:A}"
controller="$checkout/sources/SquirrelInputController.swift"
core="$checkout/sources/SquirrelAI.swift"

for source in "$controller" "$core"; do
  [[ -f "$source" ]] || {
    print -u2 -- "expected Squirrel source at $source"
    exit 1
  }
done

git -C "$checkout" cat-file -e '876adeb^{commit}' 2>/dev/null || {
  print -u2 -- "expected official Squirrel 1.1.2 base 876adeb"
  exit 1
}
git -C "$checkout" merge-base --is-ancestor 876adeb HEAD || {
  print -u2 -- "expected HEAD to descend from official Squirrel 1.1.2 base 876adeb"
  exit 1
}

ruby -ropen3 - "$controller" "$checkout" <<'RUBY'
source_path, checkout = ARGV
source = File.read(source_path)

def block_after(source, pattern, label)
  match = source.match(pattern)
  abort "missing #{label}" unless match

  opening = source.index("{", match.begin(0))
  abort "missing opening brace for #{label}" unless opening

  depth = 0
  state = :code
  escaped = false
  index = opening
  while index < source.length
    char = source[index]
    following = source[index, 2]
    case state
    when :code
      if following == "//"
        state = :line_comment
        index += 1
      elsif following == "/*"
        state = :block_comment
        index += 1
      elsif char == '"'
        state = :string
      elsif char == "'"
        state = :character
      elsif char == "{"
        depth += 1
      elsif char == "}"
        depth -= 1
        return source[match.begin(0)..index] if depth.zero?
      end
    when :line_comment
      state = :code if char == "\n"
    when :block_comment
      if following == "*/"
        state = :code
        index += 1
      end
    when :string, :character
      if escaped
        escaped = false
      elsif char == "\\"
        escaped = true
      elsif (state == :string && char == '"') || (state == :character && char == "'")
        state = :code
      end
    end
    index += 1
  end
  abort "unterminated #{label}"
end

def method(source, name)
  block_after(source, /\bfunc\s+#{Regexp.escape(name)}\b/, "method #{name}")
end

def require_match(text, pattern, message)
  abort message unless text.match?(pattern)
end

def reject_match(text, pattern, message)
  abort message if text.match?(pattern)
end

def require_order(text, first, second, message)
  first_index = text.index(first)
  second_index = text.index(second)
  abort message unless first_index && second_index && first_index < second_index
end

# Fail here on the unmodified controller: this proves RED is a missing bridge,
# not a broken harness or checkout error.
schedule = method(source, "scheduleAICandidate")
invalidate = method(source, "invalidateAICandidate")
apply = method(source, "applyAICandidate")
clear = method(source, "clearAICandidateProperties")
owns = method(source, "ownsAICandidate")
prefix_utf16 = method(source, "aiPrefixUTF16")
suffix_utf16 = method(source, "aiSuffixUTF16")
response_delegate = block_after(source, /\bclass\s+SquirrelAIURLSessionDelegate\b/, "AI URLSession delegate")

%w[Foundation Carbon InputMethodKit].each do |framework|
  require_match(source, /^import #{Regexp.escape(framework)}(?:\.[A-Za-z0-9_]+)?$/, "missing import #{framework}")
end

{
  aiDebounceTimer: /\baiDebounceTimer\s*:\s*Timer\?/,
  aiURLSession: /\baiURLSession\s*:\s*URLSession\b/,
  aiTask: /\baiTask\s*:\s*URLSessionDataTask\?/,
  aiGeneration: /\baiGeneration\s*:\s*UInt64\b/,
  aiSnapshot: /\baiSnapshot\s*:\s*SquirrelAISnapshot\?/,
  aiHistory: /\baiHistory\s*=\s*SquirrelAIHistory\(\)/,
  aiCandidates: /\baiCandidates\s*:\s*\[String\]/,
  aiCandidatePublished: /\baiCandidatePublished\s*:\s*Bool\b/,
  aiResponseDelegate: /\baiResponseDelegate\s*=\s*SquirrelAIURLSessionDelegate\(\)/,
  aiKeychainQueue: /\baiKeychainQueue\s*=\s*DispatchQueue\(/,
}.each do |name, pattern|
  require_match(source, pattern, "missing per-controller #{name} state")
end

require_match(source, /URLSessionConfiguration\.ephemeral/, "expected ephemeral URLSession configuration")
require_match(source, /timeoutIntervalForRequest\s*=\s*4(?:\.0)?/, "expected four-second request timeout")
require_match(source, /timeoutIntervalForResource\s*=\s*4(?:\.0)?/, "expected four-second resource timeout")
require_match(source, /requestCachePolicy\s*=\s*\.reloadIgnoringLocalCacheData/, "expected cache bypass")
require_match(source, /httpShouldSetCookies\s*=\s*false/, "expected cookies disabled")
require_match(response_delegate, /URLSessionDataDelegate/, "expected incremental URLSession data delegate")
require_match(response_delegate, /willPerformHTTPRedirection[\s\S]*completionHandler\(nil\)/, "expected every HTTP redirect to be rejected")
require_match(response_delegate, /NSLock\(\)/, "expected thread-safe response accumulation")
require_match(response_delegate, /taskIdentifier/, "expected response state keyed by URLSession task")
require_match(response_delegate, /didReceive\s+data:\s*Data/, "expected incremental response chunks")
require_match(response_delegate, /data\.count\s*<=\s*65_?536\s*-\s*pending\.data\.count/, "expected pre-append 64 KiB cap")
require_match(response_delegate, /dataTask\.cancel\(\)/, "expected oversized responses canceled immediately")
require_match(response_delegate, /didCompleteWithError/, "expected bounded response delivery on completion")
require_match(response_delegate, /response\.url\s*==\s*pending\.expectedURL/, "expected final response URL equality gate")
require_match(response_delegate, /200\.\.<300/, "expected HTTP 2xx gate")

require_match(schedule, /Timer\.scheduledTimer\(withTimeInterval:\s*0\.3,\s*repeats:\s*false/, "expected a nonrepeating 0.3-second debounce timer")
require_match(schedule, /NSApp\.squirrelAppDelegate\.config\?\.getString\("ai\/endpoint"\)/, "expected existing config reader for ai/endpoint")
require_match(schedule, /NSApp\.squirrelAppDelegate\.config\?\.getString\("ai\/model"\)/, "expected existing config reader for ai/model")
require_match(schedule, /SquirrelAI\.endpoint\(/, "expected endpoint validation in the scheduler")
require_match(schedule, /IsSecureEventInputEnabled\(\)/, "expected secure-input gate in the scheduler")
require_match(schedule, /aiHistory\.beginNormalComposition\(\)/, "expected explicit normal-composition history reset")
require_match(schedule, /SquirrelAI\.readAPIKey\(\)/, "expected Keychain read through the reviewed core")
require_match(schedule, /aiKeychainQueue\.async\s*\{\s*\[weak self\]/, "expected weak background Keychain callback")
require_match(schedule, /DispatchQueue\.main\.async\s*\{\s*\[weak self\]/, "expected weak return to main queue")
require_match(schedule, /SquirrelAI\.request\(/, "expected reviewed request builder")

%w[selectedRange markedRange length].each do |api|
  require_match(source, /client\.#{api}\(\)/, "expected IMK client.#{api}() API")
end
require_match(source, /client\.attributedSubstring\(from:\s*[^\)]*\)\?\.string/, "expected IMK attributedSubstring(from:) API")
require_match(source, /client\.uniqueClientIdentifierString\(\)/, "expected client identity in snapshots")
require_match(source, /prefix\(8\)/, "expected at most eight Rime candidates")
require_match(source, /prefix\(64\)/, "expected candidate text capped at 64 Characters")
require_match(prefix_utf16, /NSString/, "expected native UTF-16 prefix bounding")
require_match(prefix_utf16, /128/, "expected surrounding prefix bounded to 128 UTF-16 units")
require_match(suffix_utf16, /NSString/, "expected native UTF-16 suffix bounding")
require_match(suffix_utf16, /128/, "expected surrounding suffix bounded to 128 UTF-16 units")
require_match(source, /surroundingBefore\s*=\s*aiSuffixUTF16\(before\)/, "expected returned before-context clamped after IMK access")
require_match(source, /surroundingAfter\s*=\s*aiPrefixUTF16\(after\)/, "expected returned after-context clamped after IMK access")

require_match(source, /data\.count\s*<=\s*(?:64\s*\*\s*1024|65_?536)/, "expected 64 KiB response cap")
require_match(source, /SquirrelAI\.parseCandidate\(from:\s*data\)/, "expected strict candidate parser")
require_match(schedule, /aiResponseDelegate\.register\([\s\S]*\{\s*\[weak self\]\s+data\s+in/, "expected weak controller response completion")
reject_match(schedule, /dataTask\(with:\s*request\)\s*\{/, "completion-handler dataTask must not buffer unbounded responses")

require_match(owns, /aiGeneration\s*==\s*snapshot\.generation/, "ownership must include generation")
require_match(owns, /aiSnapshot\s*==\s*snapshot/, "ownership must include scheduled snapshot")
if schedule.scan(/guard self\.ownsAICandidate\(snapshot\) else \{ return \}/).count < 2
  abort "timer and Keychain return must silently drop stale snapshots"
end
require_match(apply, /guard ownsAICandidate\(snapshot\) else \{ return \}/, "apply must silently drop a stale snapshot")
secure_positions = schedule.enum_for(:scan, /IsSecureEventInputEnabled\(\)/).map { Regexp.last_match.begin(0) }
ownership_positions = schedule.enum_for(:scan, /guard self\.ownsAICandidate\(snapshot\) else \{ return \}/).map { Regexp.last_match.begin(0) }
unless secure_positions.length >= 3 &&
       ownership_positions.length >= 2 &&
       secure_positions[1] < ownership_positions[0] &&
       secure_positions[2] < ownership_positions[1]
  abort "secure-input checks must precede timer and Keychain ownership gates"
end
require_order(apply, "if IsSecureEventInputEnabled()", "guard ownsAICandidate(snapshot) else { return }", "secure-input check must precede apply ownership gate")

require_match(invalidate, /aiGeneration\s*&\+=\s*1/, "expected monotonic wrapping generation invalidation")
require_match(invalidate, /aiDebounceTimer\?\.invalidate\(\)/, "expected debounce cancellation")
require_match(invalidate, /aiTask\?\.cancel\(\)/, "expected task cancellation")
require_match(invalidate, /aiSnapshot\s*=\s*nil/, "expected scheduled snapshot clearing")

require_match(clear, /let\s+wasPublished\s*=\s*aiCandidatePublished/, "expected published candidate removal tracking")
require_match(clear, /aiCandidatePublished\s*=\s*false/, "published state must clear even when the session died")
require_order(clear, "aiCandidatePublished = false", "guard session", "published state must clear before session validation")
require_match(clear, /wasPublished[\s\S]*get_option\([^\n]*"_ai_refresh"/, "published removal must read refresh option")
require_match(clear, /wasPublished[\s\S]*set_option\([^\n]*"_ai_refresh"/, "published removal must rebuild the Rime menu")

{
  "_ai_candidate" => /set_property\([^\n]*"_ai_candidate"/,
  "_ai_input" => /set_property\([^\n]*"_ai_input"/,
  "_ai_generation" => /set_property\([^\n]*"_ai_generation"/,
}.each do |property, pattern|
  require_match(source, pattern, "expected transient #{property} property")
  require_match(apply, pattern, "expected applyAICandidate to set #{property}")
end
require_match(apply, /rimeAPI\.get_option\([^\n]*"_ai_refresh"/, "expected _ai_refresh read via rimeAPI")
require_match(apply, /rimeAPI\.set_option\([^\n]*"_ai_refresh"/, "expected _ai_refresh toggle via rimeAPI")
require_match(apply, /aiCandidatePublished\s*=\s*true/, "successful apply must mark the candidate published")
require_match(apply, /rimeUpdate\(scheduleAI:\s*false\)/, "expected AI apply refresh without recursive scheduling")
reject_match(apply, /\brime\./, "applyAICandidate must use rimeAPI, not nonexistent rime")

process_key = method(source, "processKey")
require_order(process_key, "invalidateAICandidate(clearProperties: false)", "rimeAPI.process_key", "processKey must invalidate before Rime processing")
require_order(process_key, "rimeAPI.process_key", "clearAICandidateProperties()", "processKey must preserve provenance through Rime processing")

select = method(source, "selectCandidate")
require_order(select, "invalidateAICandidate(clearProperties: false)", "rimeAPI.select_candidate_on_current_page", "selectCandidate must invalidate before Rime selection")
require_order(select, "rimeAPI.select_candidate_on_current_page", "clearAICandidateProperties()", "selectCandidate must preserve provenance through Rime selection")

{
  "page" => "rimeAPI.change_page",
  "moveCaret" => "rimeAPI.get_caret_pos",
}.each do |name, mutation|
  body = method(source, name)
  require_order(body, "invalidateAICandidate(clearProperties: true)", mutation, "#{name} must clear AI state before mutation")
end
page = method(source, "page")
require_match(page, /rimeUpdate\(scheduleAI:\s*false\)/, "paging must refresh UI without starting AI or resetting to page one")
require_order(page, "rimeAPI.change_page", "rimeUpdate(scheduleAI: false)", "paging must refresh the mutated page without scheduling AI")

%w[activateServer deactivateServer commitComposition createSession destroySession].each do |name|
  require_match(method(source, name), /invalidateAICandidate\(clearProperties:\s*true\)/, "#{name} must invalidate and clear AI state")
end
deinit_body = block_after(source, /\bdeinit\s*\{/, "deinit")
require_match(deinit_body, /invalidateAICandidate\(clearProperties:\s*true\)/, "deinit must invalidate AI state")
require_match(deinit_body, /aiURLSession\.invalidateAndCancel\(\)/, "deinit must invalidate the URLSession")

commit = method(source, "commit")
require_match(commit, /IsSecureEventInputEnabled\(\)/, "commit must sample secure-input state")
require_match(commit, /aiHistory\.recordCommit\(string,\s*secure:/, "commit must record only through secure history")

rime_update = method(source, "rimeUpdate")
require_match(rime_update, /func rimeUpdate\(scheduleAI:\s*Bool\s*=\s*true\)/, "rimeUpdate must normally schedule AI")
require_match(rime_update, /scheduleAI\s*\{[\s\S]*scheduleAICandidate\(\)/, "rimeUpdate must schedule after refreshing context")
require_match(rime_update, /else\s*\{[\s\S]*invalidateAICandidate\(clearProperties:\s*true\)/, "missing/empty context must invalidate AI state")

require_match(apply, /guard[\s\S]*currentAISnapshot\([\s\S]*==\s*snapshot/, "apply must rebuild and compare the full snapshot")
require_match(apply, /rimeAPI\.find_session\(/, "apply must revalidate the Rime session")
require_match(apply, /IsSecureEventInputEnabled\(\)/, "apply must revalidate secure-input state")

diff, status = Open3.capture2e(
  "git", "-C", checkout, "diff", "876adeb", "--",
  "sources/SquirrelAI.swift", "sources/SquirrelInputController.swift"
)
abort "failed to inspect added lines: #{diff}" unless status.success?
added = diff.lines.reject { |line| line.start_with?("+++") }.select { |line| line.start_with?("+") }.join
reject_match(added, /\b(?:print|debugPrint|NSLog|os_log)\s*\(/, "new AI code must not add logging")
reject_match(added, /["']sk-[A-Za-z0-9_-]{4,}/, "new AI code must not contain a hard-coded API key")
reject_match(added, /(?:getString|setString)\s*\(\s*"ai\/(?:api[_-]?key|key)"/, "API keys must not be stored in YAML config")

puts "Squirrel AI bridge contract OK"
RUBY
