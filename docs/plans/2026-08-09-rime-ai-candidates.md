# macOS Rime AI Candidates Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在 macOS 的 Squirrel 1.1.2 原生候选列表中异步加入云端 AI 第一候选，并用独立 TSV 词表学习选择结果，不阻塞或改写 Rime 原生用户词库。

**Architecture:** Rime Lua 负责候选生成、最终置顶和本地学习；基于官方 1.1.2 tag 的小型 Squirrel 补丁负责 Keychain、InputMethodKit 上下文、300 ms debounce、异步 HTTPS 和过期响应裁决。补丁以两份 `git format-patch` 存在 dotfiles 中，不 fork librime，也不引入依赖。

**Tech Stack:** Rime YAML、librime-lua、Swift 6、InputMethodKit、Security.framework、LocalAuthentication、Carbon.HIToolbox、URLSession、zsh、Ruby Fiddle、GNU Make、git format-patch。

---

## 固定边界

- Squirrel 基线必须是 tag `1.1.2`（commit 前缀 `876adeb`）；不顺手支持其他平台或版本。
- endpoint 是完整的 HTTPS Chat Completions URL；允许 query，拒绝 user/password 和非 HTTPS scheme。
- 请求仅用 `model`、`stream: false`、`messages`；响应只读 `choices[0].message.content`，其中必须是 `{"candidate":"..."}`。
- API key 只从登录 Keychain 的 generic password 读取：service `im.rime.inputmethod.Squirrel.ai`，account `default`。
- 不增加设置 UI、Accessibility 权限、重试、streaming、缓存层、数据库或第三方依赖。
- 所有 librime、IMK、AppKit 和 `IsSecureEventInputEnabled()` 调用都留在输入控制器主线程；Keychain 查询与 HTTP 不占用该线程。
- secure input 一旦被观察到，立即清空 recent commits，且该 secure 会话的 commit 永不进入后续请求。
- 检测只依赖 macOS 公共的全局 Secure Event Input；未启用该信号的自定义密码控件没有可用的 per-client IMK 标志，本版本不加 Accessibility 猜测。
- 不跟随 HTTP redirect；response body 超过 64 KiB 直接拒绝。
- 从 worktree 部署 Rime 时用 `make -C "$PWD/rime" install`。不要用顶层 `make rime`，因为 stowed `dots` 脚本硬编码了主 checkout。

## Task 1: 先固定 Rime 候选与学习契约

**Files:**

- Create: `tests/config/rime-ai-regression.lua`
- Modify: `tests/config/rime-config-regression.zsh`

**Step 1: 写会失败的结构检查**

在现有 zsh 测试中增加精确检查：

- `lua_translator@ai_learned_translator` 紧跟 `script_translator`；
- `lua_filter@ai_candidate_filter` 紧邻且位于 `uniquifier` 前；
- `squirrel.custom.yaml` 含空的 `ai/endpoint` 与 `ai/model`；
- 配置中不存在 `api_key`、`Authorization: Bearer` 或常见 `sk-...` secret。

使用 Ruby `YAML.load_file` 检查数组下标，不用脆弱的多行正则。

**Step 2: 写一个 assert 型 Lua harness**

`rime-ai-regression.lua` 只 mock `Candidate`、`yield`、context、composition、notifier 与 `rime_api.get_user_data_dir()`，直接调用生产全局组件。覆盖：

1. `_ai_input` 匹配且 generation 非空时，live AI 文本是第一个输出；
2. stale/missing property 时输入流原样通过；
3. live 文本命中前 8 个普通候选时，输出的第一项就是原 Candidate object；未命中时才创建 synthetic AI candidate；两条路径都让每个原 Candidate 恰好出现一次，除命中项移到第一外保持相对顺序；
4. 选择 live/learned 候选时只暂存，commit 后才新增或递增 TSV；取消 composition、修改完整 input 或重开 pending segment 后不写；选择普通候选且无 live AI 时不写；
5. 同一 schema/input 下按 weight、last-used、text 稳定排序；
6. 文件缺失、坏行、含 TAB/CR/LF 的字段均安全降级；
7. notifier mock 先运行 group 0 callback、再模拟 engine ungrouped callback 改变 `composition:back()`；pending 必须保留前者已复制的 input/span/candidate；
8. `fini` 会断开全部 notifier。

测试使用临时目录；不读写真实 `~/Library/Rime/ai_weights.tsv`。

**Step 3: 用 Squirrel 自带 Lua 运行 RED**

在 `rime-config-regression.zsh` 末尾用 Ruby Fiddle：

```zsh
Fiddle::Handle.new(
  "/Library/Input Methods/Squirrel.app/Contents/Frameworks/librime.1.dylib",
  Fiddle::RTLD_GLOBAL | Fiddle::RTLD_LAZY
)
```

然后从同一 Lua state 依次 `luaL_loadfilex`/`lua_pcallk` 加载：

```text
rime/rime.lua
tests/config/rime-ai-regression.lua
```

运行：

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: 非零退出，并指出缺少 `ai_learned_translator`、`ai_candidate_filter` 或 schema/config 注册。

## Task 2: 实现最小 Rime Lua 管线

**Files:**

- Modify: `rime/rime.lua`
- Modify: `rime/double_pinyin_flypy.schema.yaml`
- Modify: `rime/darwin/squirrel.custom.yaml`
- Test: `tests/config/rime-ai-regression.lua`
- Test: `tests/config/rime-config-regression.zsh`

**Step 1: 实现独立学习 translator**

在已有单文件 `rime.lua` 中增加 `ai_learned_translator` table，保持这些入口：

```lua
function ai_learned_translator.init(env)
function ai_learned_translator.func(input, seg, env)
function ai_learned_translator.fini(env)
```

持久化路径：

```lua
rime_api.get_user_data_dir() .. "/ai_weights.tsv"
```

每行格式：

```text
schema_id<TAB>input<TAB>candidate<TAB>weight<TAB>last_used_epoch
```

实现约束：

- 缺失文件与坏行直接忽略；数值必须可解析且非负；
- query 时读小文件、筛选 schema/input、按 weight 降序、timestamp 降序、text 升序；
- yield `Candidate("ai_learned", seg.start, seg._end, text, "AI")`，quality 只由存储 weight 做小幅提升；
- `init` 以 group 0 连接 `context.select_notifier`、`commit_notifier` 与 `update_notifier`（`notifier:connect(callback, 0)`），确保 Lua select callback 早于 engine 的 ungrouped slot；select 回调必须立即复制完整 input、segment start/end、schema、编码与 `get_selected_candidate():get_genuine()` 的 type/text，不能把会被 engine `Forward()` 改写的对象留到稍后再读；编码使用 `context.input:sub(segment.start + 1, segment._end)`，不能把其他 segment 一起学习；
- 仅当存在 live AI generation，或候选 type 是 `ai`/`ai_learned` 时暂存；这样选择其他候选可作为对 live AI 的纠正，但普通 Rime 输入不会被复制进 AI 表；
- 只有随后收到 commit notifier 才落盘；update notifier 在 composition 变空、完整 input 改变，或当前 back segment 仍是 pending span 但 status 已不是 selected/confirmed 时清掉 pending selection；
- 写前重读，weight 加一并封顶，写同目录 `.tmp`，close 成功后 `os.rename`；失败时保留原文件；
- `fini` disconnect 三个 notifier 并清 pending selection。

在 whole-file 读写处留下一个说明真实上限的注释：

```lua
-- ponytail: whole-file TSV is intentional for a small personal lexicon; use a DB only after measured growth makes this slow.
```

**Step 2: 实现 live AI filter**

增加：

```lua
function ai_candidate_filter(input, env)
```

它读取 `_ai_candidate`、`_ai_input`、`_ai_generation`。仅当 candidate 非空、input 等于 `context.input`、generation 非空且 composition 有 active segment 时：

1. 最多先读取 incoming 的前 8 项；
2. 若其中有同文本候选，先 yield 那个原 Candidate object，再 yield 除它自身以外的其余项；
3. 若没有同文本候选，才先 yield `Candidate("ai", segment.start, segment._end, text, "AI")`；
4. 继续 yield 其余 incoming candidates。

filter 只缓存与请求 candidate bound 相同的前 8 项，不扫描整个词库。两条分支都必须 replay 所有原候选恰好一次；match 分支只移动命中项，其他候选相对顺序不变。这样模型选择已提供候选时仍走原生 genuine candidate 与原生学习；模型生成新文本时才使用 synthetic candidate。紧随其后的原生 `uniquifier` 处理其余重复文本。无有效 live 值时完全 pass-through。

在 8 项扫描上限处记录升级条件：

```lua
-- ponytail: scan only the candidates sent to AI; raise this bound only if unseen deep duplicates are measured.
```

**Step 3: 注册组件与非 secret 配置**

在 schema 中固定：

```yaml
translators:
  - punct_translator
  - script_translator
  - lua_translator@ai_learned_translator

filters:
  - simplifier@emoji
  - simplifier@traditionalize
  - lua_filter@ai_candidate_filter
  - uniquifier
```

在已有 `rime/darwin/squirrel.custom.yaml` 的 `patch:` 下增加：

```yaml
  ai/endpoint: ""
  ai/model: ""
```

不创建 `ai.yaml`，不修改 Makefile。

**Step 4: 运行 GREEN 和真实 Rime 编译**

```zsh
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
```

Expected: exit 0，末尾打印 `Rime AI regression OK`。

隔离安装/部署：

```zsh
set -eu
rime_test_root="$(mktemp -d /tmp/dotfiles-rime.XXXXXX)"
make -C rime install \
  DIRS="$rime_test_root/cache $rime_test_root/user" \
  OS=darwin >/dev/null

'/Library/Input Methods/Squirrel.app/Contents/MacOS/rime_deployer' \
  --build "$rime_test_root/user" \
  '/Library/Input Methods/Squirrel.app/Contents/SharedSupport' \
  "$rime_test_root/build"

test -s "$rime_test_root/build/double_pinyin_flypy.schema.yaml"
rg -F 'lua_filter@ai_candidate_filter' \
  "$rime_test_root/build/double_pinyin_flypy.schema.yaml"
```

Expected: deploy exit 0，compiled schema 含 AI filter。

**Step 5: Commit**

```zsh
git add rime/rime.lua \
  rime/double_pinyin_flypy.schema.yaml \
  rime/darwin/squirrel.custom.yaml \
  tests/config/rime-ai-regression.lua \
  tests/config/rime-config-regression.zsh
git commit -m "feat(rime): add AI candidate pipeline"
```

## Task 3: 在 Squirrel 1.1.2 中建立可单独测试的 AI core

**Files:**

- Create in upstream checkout: `sources/SquirrelAI.swift`
- Modify in upstream checkout: `Squirrel.xcodeproj/project.pbxproj`
- Create in dotfiles: `tests/config/squirrel-ai-core-regression.swift`

**Step 1: 建立 pinned 临时 checkout**

```zsh
repo_root="$PWD"
squirrel_work_root="$(mktemp -d /tmp/squirrel-ai.XXXXXX)"
squirrel_checkout="$squirrel_work_root/squirrel"
git clone --recurse-submodules --branch 1.1.2 --single-branch \
  https://github.com/rime/squirrel.git "$squirrel_checkout"
cd "$squirrel_checkout"
test "$(git rev-parse --short HEAD)" = 876adeb
```

Expected: checkout 和 submodules clean，HEAD 为 Squirrel 1.1.2。

**Step 2: 写 pure-core RED check**

`tests/config/squirrel-ai-core-regression.swift` 使用 `@main` 和普通 `precondition`，不引入 XCTest。它覆盖：

- surrounding ranges：文档首尾、合法 marked range、`NSNotFound`、越界、emoji 的 UTF-16 长度；
- endpoint：只允许 HTTPS、有 host、无 embedded user/password；
- request：Bearer 只在 header，key 不在 JSON body，body 含 model/messages/`stream: false`；
- parser：正常 outer/inner JSON；empty choices、nil、Markdown fence、空值、换行、control、超过 64 characters 均拒绝；
- stale gate：generation、session、schema、input、caret、app ID、候选或周边文本任一改变即不匹配；
- secure history：观察到 secure 后立即清空；随后到达的 delayed secure commit 不记录；下一次明确的 normal composition 从空 history 重新开始；
- 用随机不存在 service 验证 Keychain missing 会返回 nil 且不弹 UI。

先运行：

```zsh
xcrun swiftc -parse-as-library \
  "$squirrel_checkout/sources/SquirrelAI.swift" \
  "$repo_root/tests/config/squirrel-ai-core-regression.swift" \
  -framework Security \
  -framework LocalAuthentication \
  -o /tmp/squirrel-ai-core-regression
```

Expected: 因 `SquirrelAI.swift` 尚不存在而失败。

**Step 3: 实现一个无抽象层的 core 文件**

`sources/SquirrelAI.swift` 只放实际共用逻辑：

- 只含 Foundation primitive 的 `SquirrelAISnapshot: Equatable`（session 存 `UInt64`，不依赖 Squirrel bridging types）；
- 保存 recent commits 与 secure-taint 状态的 `SquirrelAIHistory` value type，提供 `observeSecureInput()`、`beginNormalComposition()`、`recordCommit(_:secure:)`；
- 安全的 UTF-16 surrounding range 计算；
- endpoint/request builder；
- Chat Completions outer response 与 `{"candidate": ...}` inner response decoder；
- 单行、非 control、`1...64` character candidate validator；
- generic-password Keychain reader。

Keychain query 必须包含：

```swift
let context = LAContext()
context.interactionNotAllowed = true

let query: [CFString: Any] = [
  kSecClass: kSecClassGenericPassword,
  kSecAttrService: "im.rime.inputmethod.Squirrel.ai",
  kSecAttrAccount: "default",
  kSecMatchLimit: kSecMatchLimitOne,
  kSecReturnData: true,
  kSecUseAuthenticationContext: context,
]
```

不要加入 `kSecUseDataProtectionKeychain`。所有失败只返回 nil/typed error，不包含 response body、context、header 或 key。

把该文件加入 Squirrel app target；不创建新 target、protocol、client interface 或依赖。

**Step 4: 运行 core GREEN**

```zsh
xcrun swiftc -parse-as-library \
  "$squirrel_checkout/sources/SquirrelAI.swift" \
  "$repo_root/tests/config/squirrel-ai-core-regression.swift" \
  -framework Security \
  -framework LocalAuthentication \
  -o /tmp/squirrel-ai-core-regression
/tmp/squirrel-ai-core-regression
git diff --check
```

Expected: `Squirrel AI core regression OK`，`git diff --check` 无输出。

**Step 5: Commit upstream patch 1**

```zsh
git add sources/SquirrelAI.swift Squirrel.xcodeproj/project.pbxproj
git commit -m "feat: add squirrel ai request core"
```

## Task 4: 把异步结果安全接回原生 Rime 候选流

**Files:**

- Modify in upstream checkout: `sources/SquirrelInputController.swift`
- Create in dotfiles: `tests/config/squirrel-ai-bridge-regression.zsh`

**Step 1: 写 bridge RED contract**

脚本接收一个 Squirrel checkout 路径，并按具体 Swift 方法体断言 controller 包含且调用：

- `scheduleAICandidate`、`invalidateAICandidate`、`applyAICandidate`；
- 300 ms timer；
- `IsSecureEventInputEnabled()`；
- `client.selectedRange()`、`client.markedRange()`、`client.length()`、`client.attributedSubstring(from:)`；
- `_ai_candidate`、`_ai_input`、`_ai_generation`、`_ai_refresh`；
- `URLSessionConfiguration.ephemeral`；
- activate/deactivate、commit、candidate/page/caret、create/destroy lifecycle 中的 invalidate 调用。

脚本对 `git diff 876adeb -- sources/SquirrelAI.swift sources/SquirrelInputController.swift`（包含尚未 commit 的工作区改动）的新增行拒绝 `print`/`debugPrint`/`NSLog`/`os_log`、request/response body logging 与硬编码 `sk-`；不能因 1.1.2 原文件已有的 `print` 而失败。名称 grep 只做 smoke check，生命周期位置以对应方法体检查和 app build 为准。

```zsh
/bin/zsh "$repo_root/tests/config/squirrel-ai-bridge-regression.zsh" \
  "$squirrel_checkout"
```

Expected: 非零退出，报告缺少 bridge hooks。

**Step 2: 添加每 controller 的最小状态**

只在 `SquirrelInputController` 保存：debounce timer、ephemeral `URLSession`、`URLSessionDataTask`、generation、last scheduled snapshot、`SquirrelAIHistory`、最近候选文本。history 内部保存最近 5 次非空 commit、每次最多 128 characters，并跟踪 secure taint；当前请求只带前 8 个候选、每个最多 64 characters。不要放到 AppDelegate 或全局 singleton。

统一 `invalidateAICandidate(clearProperties:)`：

1. generation `&+= 1`；
2. invalidate timer；
3. cancel task；
4. 清 last snapshot；
5. 需要时把三个 transient property 设为空。

在按键导致 composition 变化、candidate/page/caret 操作、activate/deactivate、commit/cancel、session create/destroy 和空 composition 时调用。`deinit` 也取消 timer/task 并 invalidate URLSession。处理按键时先递增 generation 并取消 timer/task，但要等 `processKey` 返回后再清 property；这样 Space/数字键触发的 Lua `select_notifier` 仍能看到这次选择来自 live AI 列表。page/caret/client 生命周期操作则立即清 property。

任何 secure check 命中时必须同时调用 `history.observeSecureInput()`、取消请求并清 transient properties。统一 commit path 调 `history.recordCommit(_:secure:)`；当前 secure signal 或 history 的 secure taint 任一为 true 都不记录文本。只有后续一次明确的非 secure composition 调度开始时才调用 `history.beginNormalComposition()`；该方法先再次清空 history，再解除 taint。这防止 secure focus/commit 回调时序把密码带入普通请求。

**Step 3: 采集主线程 snapshot 并 debounce**

从现有 `SquirrelConfig` 直接读取：

```swift
NSApp.squirrelAppDelegate.config?.getString("ai/endpoint")
NSApp.squirrelAppDelegate.config?.getString("ai/model")
```

没有 endpoint/model、没有 active session、空 input 或 secure event input 时直接退出。snapshot 至少包含：

- session ID、generation、schema、raw input、caret；
- controller 已有的稳定 `currentApp`（来源是 `client.bundleIdentifier()`）；不要调用每次都会生成新值的 `client.uniqueClientIdentifierString()`；
- 当前前 8 个候选文本，每项最多 64 characters；
- 最近 5 次 commit，每项最多 128 characters；
- marked range 外、前后各最多 128 UTF-16 units 的 surrounding text。

范围计算使用 core helper。调用正确的 Squirrel 1.1.2 `IMKTextInput` API：

```swift
client.selectedRange()
client.markedRange()
client.length()
client.attributedSubstring(from: range)?.string
```

任何 `NSNotFound`、越界或 nil substring 都只让 surrounding text 退化为空。raw input 超过 64 characters 时不请求。第一次 snapshot 后用一次 300 ms `Timer`；同一 snapshot 不重复调度。

**Step 4: 后台读 Keychain 并异步发送**

Keychain 查询放 dedicated serial queue。结果回主线程再检查 generation/snapshot/session/secure 状态；仍有效才创建 ephemeral `URLSession` task。固定 4 秒 timeout、无 retry、无 stream，并用一个最小 `URLSessionTaskDelegate` 拒绝所有 redirect；用户必须配置最终 URL。

HTTP 完成后要求最终 response 仍是配置 URL、HTTP 2xx 且 body 不超过 64 KiB；只把 status category 或已验证 candidate 送回主线程。绝不输出请求、响应、上下文或 Authorization header。

**Step 5: 主线程执行完整 stale gate 并刷新**

完成回调 weak capture controller，并重新检查：controller 存活、generation、`find_session`、session、schema、input、caret、app ID、候选、surrounding text 和 secure state。

通过后：

```swift
rimeAPI.set_property(session, "_ai_candidate", candidate)
rimeAPI.set_property(session, "_ai_input", snapshot.input)
rimeAPI.set_property(session, "_ai_generation", String(snapshot.generation))
rimeAPI.set_option(session, "_ai_refresh", !rimeAPI.get_option(session, "_ai_refresh"))
rimeUpdate(scheduleAI: false)
```

让现有 `rimeUpdate` 接受默认 `scheduleAI: true`，AI apply 唯一传 false，避免刷新再发请求。`rimeConsumeCommittedText`/统一 `commit(string:)` 只维护有界 recent commits。

**Step 6: 运行 bridge check 与构建**

```zsh
/bin/zsh "$repo_root/tests/config/squirrel-ai-bridge-regression.zsh" \
  "$squirrel_checkout"
cd "$squirrel_checkout"
bash ./action-install.sh
make debug
git diff --check
```

Expected: bridge check exit 0，`make debug` 成功，diff check 无输出。

**Step 7: Commit upstream patch 2**

```zsh
git add sources/SquirrelInputController.swift
git commit -m "feat: bridge ai candidates into rime"
```

## Task 5: 保存可重放补丁与操作文档

**Files:**

- Create: `rime/squirrel-ai/patches/0001-feat-add-squirrel-ai-request-core.patch`
- Create: `rime/squirrel-ai/patches/0002-feat-bridge-ai-candidates-into-rime.patch`
- Create: `rime/squirrel-ai/README.md`
- Test: `tests/config/squirrel-ai-core-regression.swift`
- Test: `tests/config/squirrel-ai-bridge-regression.zsh`

**Step 1: 导出两份 patch**

从临时 upstream checkout：

```zsh
mkdir -p "$repo_root/rime/squirrel-ai/patches"
git format-patch -2 \
  --output-directory "$repo_root/rime/squirrel-ai/patches"
```

Expected: 两份按顺序编号的 patch；patch 中不含 key、真实 endpoint、build products。

**Step 2: 写最短可操作 README**

README 只记录：

1. pinned clone 与 `git am` 两份 patch；
2. `bash ./action-install.sh && make debug`；
3. 安装前备份/确认，随后只替换精确的 `Squirrel.app` 并运行 upstream postinstall；禁止调用会递归 `chown` 整个 `/Library/Input Methods` 的 `make install-debug`；
4. 在 `squirrel.custom.yaml` 填非 secret endpoint/model 后，从当前 checkout 执行 Rime install/deploy；
5. 安装自编译 app 后再交互写 key；签名/build 改变时重跑；
6. key 删除命令、AI TSV 路径、回滚到备份 app；
7. 升级 Squirrel 时从新 tag 建 clean checkout、`git am`、修冲突、跑同一组检查，不能直接假设 patch 兼容。

Key 写入命令必须逐字保留，且 `-w` 最后：

```zsh
/usr/bin/security add-generic-password \
  -U \
  -a default \
  -s im.rime.inputmethod.Squirrel.ai \
  -T "/Library/Input Methods/Squirrel.app" \
  -w
```

删除命令：

```zsh
/usr/bin/security delete-generic-password \
  -a default \
  -s im.rime.inputmethod.Squirrel.ai
```

**Step 3: 从全新 tag 重放验证**

```zsh
squirrel_verify_root="$(mktemp -d /tmp/squirrel-ai-verify.XXXXXX)"
squirrel_verify_checkout="$squirrel_verify_root/squirrel"
git clone --recurse-submodules --branch 1.1.2 --single-branch \
  https://github.com/rime/squirrel.git "$squirrel_verify_checkout"
git -C "$squirrel_verify_checkout" am \
  "$repo_root"/rime/squirrel-ai/patches/*.patch

xcrun swiftc -parse-as-library \
  "$squirrel_verify_checkout/sources/SquirrelAI.swift" \
  "$repo_root/tests/config/squirrel-ai-core-regression.swift" \
  -framework Security \
  -framework LocalAuthentication \
  -o /tmp/squirrel-ai-core-regression
/tmp/squirrel-ai-core-regression
/bin/zsh "$repo_root/tests/config/squirrel-ai-bridge-regression.zsh" \
  "$squirrel_verify_checkout"
```

Expected: 两个 patch clean apply，两个 regression checks 均通过。

**Step 4: Commit dotfiles artifacts**

```zsh
cd "$repo_root"
git add rime/squirrel-ai \
  tests/config/squirrel-ai-core-regression.swift \
  tests/config/squirrel-ai-bridge-regression.zsh
git commit -m "feat(rime): package Squirrel AI bridge"
```

## Task 6: 最终自动验证与受控安装

**Files:**

- Verify only; runtime key and `~/Library/Rime/ai_weights.tsv` must remain untracked.

**Step 1: 跑仓库与隔离构建检查**

```zsh
cd "$repo_root"
/bin/zsh -n tests/config/rime-config-regression.zsh
/bin/zsh tests/config/rime-config-regression.zsh
/bin/zsh tests/config/squirrel-ai-bridge-regression.zsh \
  "$squirrel_verify_checkout"
/tmp/squirrel-ai-core-regression
git diff --check
git status --short
```

再在 fresh patched checkout 执行：

```zsh
cd "$squirrel_verify_checkout"
bash ./action-install.sh
make debug
```

Expected: 所有检查通过，dotfiles worktree clean，Squirrel debug build 成功。

**Step 2: 安装是单独的破坏性 checkpoint**

停止并向用户确认后，才备份和替换当前输入法。先验证 build product 与两个绝对路径，再只操作精确 app bundle；不要运行 upstream `make install-debug`，因为它的 `permission-check` 会对整个 `/Library/Input Methods` 递归 `chown`：

```zsh
squirrel_backup_root="$(mktemp -d /private/tmp/squirrel-backup.XXXXXX)"
squirrel_built_app="$squirrel_verify_checkout/build/Build/Products/Debug/Squirrel.app"
test -d "$squirrel_built_app/Contents/MacOS"
test -d "/Library/Input Methods/Squirrel.app/Contents/MacOS"

/usr/bin/sudo /bin/mv \
  "/Library/Input Methods/Squirrel.app" \
  "$squirrel_backup_root/Squirrel.app"
/usr/bin/sudo /usr/bin/ditto \
  "$squirrel_built_app" \
  "/Library/Input Methods/Squirrel.app"

cd "$squirrel_verify_checkout"
DSTROOT="/Library/Input Methods" RIME_NO_PREBUILD=1 \
  /bin/bash scripts/postinstall
```

报告实际 backup 路径。复制或 postinstall 失败时，用同一 backup 恢复精确 app bundle。不要在未确认时运行任何替换命令。

**Step 3: 配置、写 key、部署**

用户填入真实 endpoint/model 后，从当前 dotfiles checkout 部署：

```zsh
cd "$repo_root"
make -C "$repo_root/rime" install
```

然后执行 README 中的交互 Keychain 命令。key 不得通过参数、环境变量、文件或聊天传入；安装/签名变化后重跑命令。最后 reload Squirrel。

**Step 4: 手工验收**

在 TextEdit、Safari/Chrome 和 VS Code/Electron 各验证：

- 本地候选即时出现，AI 不阻塞输入；
- 停顿 300 ms 后最多一个请求；
- AI 结果无需再按键就成为原生候选第一项，Space/数字键/鼠标选择正确；
- 相同文本不重复显示；
- 连续快速输入、移动 caret、翻页、切 App、切 schema、取消 composition 时旧响应永不闪现；
- timeout、HTTP 非 2xx、坏 JSON、缺 key 时普通 Rime 不受影响且无认证弹窗；
- 原生与浏览器密码框不发请求；
- 不提供 surrounding text 的 App 仍可用候选与 recent commits；
- 接受 AI、再选择纠正候选、重启 Squirrel 后，`ai_weights.tsv` 的排序仍生效；
- logs 中没有 key、Authorization、上下文和完整响应。

**Step 5: 最终证据**

记录：Squirrel build 成功行、Rime isolated deploy 成功、两个 regression 输出、三类 App 的 AI 第一候选结果、密码框零请求、backup 路径。不要提交真实 endpoint（如果用户不想公开）、API key、运行时 TSV 或 build artifacts。
