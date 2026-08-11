# Rime 中英文自动空格设计

## 目标

在中文模式中，通过 Rime 候选连续上屏中文与英文时，自动在直接相邻的汉字和 ASCII 英文字母之间插入一个半角空格。

目标输入流程：

```text
一个 -> harness -> 来
```

目标结果：

```text
一个 harness 来
```

## 已确认范围

- 只处理同一 Rime 会话中、没有夹杂宿主编辑操作的连续候选上屏文本。
- 英文通过中文模式的候选路径输入；当前 `melt_eng` 是主要入口，但规则同样适用于 AI、自定义短语等其他候选。只排除 ASCII 模式直通。
- 只把 `[A-Za-z]` 视为英文边界；数字不触发自动空格。
- 只在相邻提交之间增加候选前缀，不改写候选内部文本。
- 不读取或改写宿主应用中的现有正文。
- 暂不增加可见开关；schema 是否挂载过滤器决定功能是否启用。

## 方案选择

采用 Lua filter 读取 Rime `commit_history:back()` 的带类型提交记录，并以 `UniquifiedCandidate` 种子和后续 `ShadowCandidate` 为当前候选增加前导空格。

选择该方案的原因：

- 继续走 Rime 的正常候选选择与提交路径；
- genuine candidate 保持不变，便于保留用户词频、候选类型和现有 AI 学习语义；
- 不需要枚举空格、数字键、回车等所有选词按键；
- 不需要修改或重新构建 Squirrel 前端。

未采用的方案：

1. Lua processor 拦截选词键并调用 `engine:commit_text()`。该方案需要覆盖所有提交入口，容易绕过正常用户词典更新，并会与现有 processor 行为耦合。
2. 在 Squirrel 提交出口读取应用光标周围文本并统一格式化。该方案能覆盖更多编辑场景，但仅限 macOS，且会增加定制 Squirrel 补丁的维护成本，超出本功能范围。

## 组件与顺序

在 `rime/rime.lua` 中增加 `auto_space_filter`。在 `rime/double_pinyin_flypy.schema.yaml` 中把它放在简繁转换和 AI 过滤器之后、内建去重器之前，并使 `uniquifier` 严格位于 filter 尾部：

```yaml
filters:
  - simplifier@emoji
  - simplifier@traditionalize
  - lua_filter@ai_candidate_filter
  - lua_filter@auto_space_filter
  - uniquifier
```

该顺序保证：

- 普通、英文、AI 和自定义短语候选都经过相同的边界规则；
- 自动空格文本变换发生在简繁转换和 AI 注入之后，使用用户实际看到的候选文本；
- 内建 `uniquifier` 作为最后一层，按已加空格的最终文本去重；
- AI filter 在注入前只反解能够由同一 typed-history 边界规则严格证明的一个展示空格，随后仍由自动空格 filter 统一生成最终显示文本。

librime 的 Menu 会保存最外层 filter 已输出的候选，而 `UniquifiedTranslation` 是惰性的：它在外层 Lua filter 继续迭代时，使用自己的内层下一候选文本与共享 Menu 比较。若顺序为 `uniquifier -> auto_space_filter`，Menu 中已是 `" Rime"`，内层却仍以 `"Rime"` 比较，重复候选会泄漏。因此内建去重器必须最后运行。

但若自动空格的第一个输出只是 Shadow，最终去重器发现第二个同文本候选时，会把 Menu 中的第一项替换为逻辑类型 `uniquified` 的 `UniquifiedCandidate`，从而丢失 `auto_space` 来源标记。过滤器因此必须对每个首次出现的安全加空格文本预先构造 `UniquifiedCandidate(ultimate, "auto_space", spaced, final_comment)`。后续同文本自动候选输出 `ShadowCandidate(ultimate, "auto_space", spaced, final_comment)`，最终去重器看到前项已是 dynamic `Uniquified`，只会 append，不会改写其逻辑类型。

librime 的 `get_genuine()` 一次只会从 `UniquifiedCandidate` 取第一项并继续解开一层 `ShadowCandidate`，`get_genuines()` 则按 append 顺序对每项各解一层 Shadow。过滤器在构造种子前，仍必须通过 `get_dynamic_type()` 和有上限的迭代取得最底层 genuine candidate：只对 `Shadow`/`Uniquified` 调用 `get_genuine()`，遇到 `Sentence`/`Phrase`/`Simple`/`Other` 立即停止。librime-lua 每次为返回的 C++ `shared_ptr<Candidate>` 创建新 userdata，且 Candidate metatable 没有 `__eq`，因此 Lua identity 不能用作原生固定点证明。此结构保留最终 display/comment，同时使 `get_genuine()` 和扁平 `get_genuines()` 都返回无排版空格的 ultimate candidates。

## 数据流

在自动空格 filter 之前，`ai_candidate_filter` 先按原始 live `_ai_candidate` 文本查找 incoming Candidate；精确命中可能代表自然前导空白，必须原样提升。只有没有精确命中时，才检查 live 文本是否携带可证明的自动展示前缀：active segment 与候选覆盖相同的首段 span、live 文本以恰好一个 U+0020 开头、typed history 与去前缀文本满足同一 Han ↔ ASCII 规则，而且无空格同文候选本身通过 ultimate genuine、span 与 comment 的安全包装检查。首个同文候选不安全时继续扫描后续项。没有安全无空格同文项时保留 live 文本构造 synthetic candidate，不猜测空格来源。这样已知展示往返会恢复原 Candidate，而自然或歧义空格仍 fail closed。

过滤器对每个候选执行以下步骤：

1. 只考虑 `candidate.start == 0` 的候选，避免把同一次 composition 的后续 segment 错当成跨提交边界。
2. 在同一个 `pcall` 中恰好一次读取 `context.commit_history:back()` 及其 `type/text`；不读取会丢失记录类型的 `latest_text()`。
3. 只接受非空字符串记录类型，并排除保留类型 `thru` 和 `raw`；再取该记录文本最后一个 Unicode 字符和当前候选第一个 Unicode 字符。
4. 仅当边界为“汉字 -> ASCII 字母”或“ASCII 字母 -> 汉字”时需要空格。
5. 需要空格时，读取 `get_dynamic_type()`；终态类型直接作为 genuine，只有 `Shadow`/`Uniquified` 才以有上限、带循环检测的迭代逐层解包。
6. 每次 filter coroutine 维护一个按“实际输出文本”键控的来源表。所有原样通过和 fail-closed 输出都只在键尚未出现时记为 `other`。
7. 只有 genuine candidate 与最终显示候选的 `start/_end` 相同，且不存在“最终 comment 为空、genuine comment 非空”的继承冲突时，才计算 `spaced = " " .. candidate.text`：若 `spaced` 未出现，用 ultimate genuine 构造逻辑类型 `auto_space` 的 `UniquifiedCandidate` 种子，成功后记为 `auto`；若已为 `auto`，输出一层 source 同样为 ultimate genuine 的 `ShadowCandidate`；若已为 `other`，原样输出未加空格候选。
8. 种子构造抛错或返回非 candidate 时，同时把 `spaced` 标记为 `other` 并输出原候选，阻止后续重复项在同一 coroutine 中从未加空格切换为加空格。只有成功种子才能标记 `auto`。
9. 两个构造器的返回值都必须是 table/userdata。`ShadowCandidate` 不依赖 `inherit_comment` 参数；当前 librime-lua 绑定接受该参数但没有把它传给 C++ 构造器，因此 comment 必须显式传入。若空 comment 会因默认继承而恢复 genuine comment，则选择不包装，避免改变候选注释。
10. 最终内建去重器按加空格后的文本聚合重复项，保留首次出现的 display/comment 和位置，把 quality 提升到各项最大值，并使 `get_genuines()` 保持 ultimate candidates 的遇到顺序。不对候选内部做全局替换。

例如：

```text
候选提交记录末尾「个」 + 候选「harness」 -> 「 harness」
候选提交记录末尾「s」  + 候选「来」      -> 「 来」
```

汉字检测使用 Unicode 17.0 的下列 CJK 统一及兼容汉字范围，而不是把所有非 ASCII 字符都视为中文：

- `U+3400–U+4DBF`
- `U+4E00–U+9FFF`
- `U+F900–U+FAFF`
- `U+20000–U+2FA1F`
- `U+30000–U+3347F`

其中较宽的补充平面范围包含扩展 B–J、兼容汉字及其保留空位；只有实际出现在候选中的码点才会参与判断。英文检测只接受 ASCII 大小写字母。

## 边界规则

应插入空格（“上次提交”均来自非 `thru`/`raw` 的候选记录）：

| 上次提交 | 当前候选 | 输出候选 |
|---|---|---|
| `中文` | `Rime` | ` Rime` |
| `Rime` | `输入法` | ` 输入法` |

应保持原样：

| 上次提交 | 当前候选 | 原因 |
|---|---|---|
| `第` | `3次` | 数字不属于英文边界 |
| `2026` | `年` | 数字不触发空格 |
| `中文 ` | `Rime` | 已有空白，不重复插入 |
| `中文，` | `Rime` | 不跨标点插入 |
| `中文🙂` | `Rime` | 不跨 Emoji 插入 |
| 空历史 | 任意候选 | 没有可靠边界 |
| 直接键入 `R`（`thru`） | `输入法` | 不是候选提交 |
| `raw` 记录 | Han/ASCII 候选 | 无法证明来自候选提交 |

URL、邮箱、`C++`、版本号等候选内部内容不做改写。如果整个候选以 ASCII 字母开头，仍可在它与前一个汉字之间增加一个空格。

## 失败策略

实现不维护跨应用私有状态，只使用 Rime 自己的提交历史。以下情况一律原样输出：

- `commit_history:back()` 报错或返回空记录；
- 历史记录的 `type/text` 读取报错、类型非法或文本为空；
- 历史记录类型为 `thru` 或 `raw`；
- 历史或候选文本为空；
- UTF-8 字符无法可靠解析；
- 任一边界字符不属于明确的 Han 或 ASCII 字母类别；
- 候选不是 composition 的首段（`candidate.start ~= 0`）；
- 上游候选已经带有自动空格包装或自然前导空白；live AI property 中满足上一节全部证明条件的单个 U+0020 展示前缀是唯一例外；
- dynamic type 查询报错、为空、不是字符串或不属于明确的 wrapper/终态白名单；
- genuine 解包报错、返回无效值、形成循环、超过深度上限或改变候选 span；
- 最终 comment 为空但 genuine comment 非空，无法在当前绑定中无损包装；
- `UniquifiedCandidate`/`ShadowCandidate` 构造抛错、返回 `nil` 或非 candidate；
- 同一 coroutine 中已有原样输出占用目标加空格文本，例如自然前导空格候选先于 `Rime` 出现时，后者保守地以未加空格文本输出。

Rime 会把无修饰的可打印 ASCII（包括手动空格）记录为 `thru`，把未翻译的 composition 片段及 `engine:commit_text()` 记录为 `raw`。两者都不能证明文本来自候选提交，因此一律排除：直接 ASCII 之后的 Han 候选、以及任一方向的 `raw` 记录边界都故意不自动补空格。无修饰退格或回车会清空提交历史，此后同样不插入。

鼠标移动光标、粘贴、切换应用以及宿主应用自行修改文本对 librime-lua 不可见。发生这些操作后，过滤器无法确认光标前的真实字符，可能仍看到旧的 Rime 提交记录；这些场景明确不保证自动空格结果。实现不额外缓存或伪装成能够读取宿主正文，并在所有可检测的异常中原样输出。

## 现有功能兼容

### 以词定字

当前 `select_character` 使用 `context:get_commit_text()` 选择首字或尾字。最终去重后的自动空格候选是 dynamic `Uniquified`、逻辑类型 `auto_space`，并带一个前导空格。该 processor 必须从 `context.composition:toSegmentation()` 取得首 segment 的实际选中候选及其 `get_genuine()` 结果，而不能只查看当前活动候选。只有该候选类型严格为 `auto_space`、存在有效 genuine candidate、显示文本以恰好一个自动前缀开头，且完整 commit text 确实以该显示文本开头时，才能确认它来自自动包装；不能仅凭文本以空格开头来猜测来源。这里不要求 display 等于空格加 genuine 文本，因为简繁转换后的最终 display 可能与 genuine 文本不同。

确认包装关系后：

- 从完整 commit text 开头移除且仅移除这个已证明的自动前缀，再选择首字或尾字；
- 提交时重新保留该前缀；
- `Ctrl+J/L` 不能把空格本身当作目标字符。

该追溯同样适用于首 segment 已确认、当前活动候选位于后续 segment 的 composition。若首 segment 无选中候选，或类型、genuine、显示文本和完整 commit text 不能共同证明存在自动包装，则保持原有以词定字逻辑。自动空格本身只包装 `candidate.start == 0` 的候选，因此不会在后续 segment 中产生新的自动前缀。

当前 `Ctrl+J/L` 通过 `engine:commit_text()` 提交，所以该次历史记录为 `raw`。为严格维持“候选提交之间”的边界，其后的候选将保守地不自动补空格；若要改变此行为，必须在后续 Task 4 变更中显式保留可验证的候选来源，不能把所有 `raw` 都视为候选。

### AI 候选学习

现有学习路径对最终选中候选调用 `get_genuine()`。自动空格种子的第一项直接是 ultimate genuine，后续重复项则是只包装该 ultimate 的一层 Shadow；因此最终 dynamic `Uniquified` 候选的单次 `get_genuine()` 取得第一个无排版空格的原始候选，`get_genuines()` 也按顺序得到扁平 ultimate 列表。自动空格只影响最终显示与上屏文本，不应进入 AI 学习 TSV。

Squirrel 的 AI snapshot 只能通过 public C API 读取最终 Menu 的显示文本，所以自动前缀也会随候选发送给模型；response parser 与 `_ai_candidate` property 会原样保留它。`ai_candidate_filter` 因此必须把已知展示往返映射回语义候选，但不能盲目 trim。它先在前 8 项中查找与 live 文本完全相同的 Candidate 并原样提升；只有未精确命中、live 文本以恰好一个 U+0020 开头、active segment 从 0 开始、`commit_history:back()` 是有效且非 `thru`/`raw` 的候选记录，并且去掉该空格后满足现有 Han ↔ ASCII 字母规则时，才查找可安全包装的无空格同文候选。找到时直接提升该原 Candidate，保留 genuine/type/span/comment/quality；找不到时以原 live 文本构造 synthetic candidate。任一证明失败时同样保留 live 文本不变。

### 用户词典与排序

普通候选仍通过原有选择路径提交。自动空格过滤器先把已有 `UniquifiedCandidate` 和 Shadow 层归一到最底层 genuine，再输出一个 `auto_space` Uniquified 种子及后续单层 Shadow，由最终内建去重器聚合。内建 append 保留首次出现位置和 display/comment，并把 quality 提升到各项最大值；但 mocked Lua Menu 测试和部署成功都不足以证明真实用户词典语义或原生惰性去重。验证必须在实际 Rime 会话中确认重复候选只剩一项、逻辑类型仍可被 `Ctrl+J/L` 识别、comment 与排序没有退化，且用户词频继续写入无前导空格的 genuine 词条。public Rime API 不暴露原始 quality，因此真实会话不能据此宣称 quality 数值完全相等。

## 验证

### Lua 行为回归

新增 `tests/config/rime-auto-space-regression.lua`，由现有 `tests/config/rime-config-regression.zsh` 使用 Squirrel 自带 Lua 加载生产 `rime.lua` 后执行。该 harness 使用 mock 验证 Lua 判断、包装和共享 Menu 惰性去重契约，不把结果当作真实 C++ `UniquifiedCandidate`/`ShadowCandidate`、原生 Menu 或用户词典行为证明。覆盖：

- 汉字到英文、英文到汉字；
- 普通历史使用候选记录类型，任意非空、非保留类型均可接受；
- 直接 ASCII `thru` 及 Han↔ASCII 两方向的 `raw` 记录都原样通过；
- `back()` 恰好读取一次，`latest_text()` 即使抛错或返回误导边界也绝不读取；
- `back()` 或记录 `type/text` 访问异常、空值及非法类型均 fail closed；
- 数字、标点、已有空白、Emoji、空历史；
- 不重复包装；
- 仅处理 `candidate.start == 0`，后续 segment 原样输出；
- 对 simplifier/uniquifier 风格的嵌套候选逐层归一，种子和后续 Shadow 的 source 都是最底层 genuine candidate；
- native-like 终态即使 `get_genuine()` 会返回新 alias 或抛错也不得调用该方法，并通过 dynamic type 直接终止；
- 自动包装保留最终显示文本、comment、候选数量和顺序；
- 两个、三个及交错出现的同文本加空格候选在最终 Menu 中每种文本只剩一项，逻辑类型为 `auto_space`、dynamic type 为 `Uniquified`；
- 种子第一项是 ultimate1，后续项为非嵌套的单层 Shadow，`get_genuine()` 取 ultimate1，`get_genuines()` 以 ultimate1…n 顺序扁平返回；
- 两种不同加空格文本各自建立种子，非加空格重复项保持内建去重行为；
- 首个自然前导空格候选占用目标文本时，后续自动目标 fail closed 为未加空格文本；
- 种子和后续 Shadow 构造抛错、返回 `nil`/非 candidate 时原样输出，种子失败后同文本不重试加空格；
- 模拟 Lua 迭代器、最外层输出和共享 Menu 的先后顺序，并验证去重后的实际候选可被 `Ctrl+J/L` 与 AI 学习消费；
- 模拟 Squirrel 把带自动前缀的最终 Menu 文本经模型原样回灌 `_ai_candidate`，覆盖两种边界方向、匹配原 Candidate、自然空格 exact match、歧义无匹配 fail-closed、最终去重以及 AI TSV 只递增无空格语义行；
- 缺失/`raw`/`thru` 历史、非 Han ↔ ASCII 边界、多个首空格和非首 segment 均不得反解 live 文本；
- dynamic type 异常/未知，以及 genuine 链循环、超深、异常、无效返回或 span 不一致时原样通过；
- 空 display comment 会被非空 genuine comment 继承时原样通过；
- `Ctrl+J/L` 对带自动前缀的候选正确提交；
- 两段 composition 中当前活动候选位于第二段时，`Ctrl+J/L` 仍从首 segment 识别并保留唯一自动前缀；
- mocked AI 候选及学习记录使用无前导空格的 genuine 文本。

### 配置回归

扩展 `tests/config/rime-config-regression.zsh`：

- 检查自动空格 harness 存在；
- 断言 filter 各出现一次，尾序严格为 `ai_candidate_filter -> auto_space_filter -> uniquifier`，且 `uniquifier` 是绝对最后一项；
- 使用现有 bundled-Lua 运行器加载生产 Lua 与两个测试 harness。

### 静态和原生部署

- `zsh -n tests/config/rime-config-regression.zsh`
- `tests/config/rime-config-regression.zsh`
- Ruby/Psych YAML 解析
- `git diff --check`
- 临时 HOME 下执行 Rime 安装和原生 `rime_deployer --build`
- 检查部署和 Squirrel 日志中没有 Lua、编码或组件加载错误

原生部署只证明组件能够加载和编译配置，不证明原生 Menu 的惰性去重、`UniquifiedCandidate`/Shadow 解包、用户词典学习或真实提交行为；这些由下一节的实际会话验收承担。

### 真实输入验收

在临时部署的 Squirrel 中验证：

```text
一个 -> 候选 harness -> 候选来 = 一个 harness 来
候选 Rime -> 候选输入法       = Rime 输入法
直接键入 R -> 候选输入法    = R输入法
第 -> 3次                = 第3次
中文 -> 手动空格 -> Rime = 中文 Rime
中文，-> Rime            = 中文，Rime
```

另测 `Ctrl+J/L`、退格、回车和 AI 候选，并为能从两个、三个及交错来源产生同一最终加空格文本的真实输入编码检查 Menu：每种文本只能显示一个候选，在该实际去重结果上 `Ctrl+J/L` 仍必须保留前缀，AI 学习仍必须使用无空格 genuine 文本。确认测试结果不宣称覆盖鼠标移动、粘贴或宿主应用改文。部署到当前用户的 Rime 目录不属于仓库实现，除非另行明确授权。

使用真实普通词典候选重复输入，确认候选排序与用户词频仍能正常学习，且用户词典没有产生带前导空格的词条。使用真实 AI 与 `ai_learned` 候选，确认上屏文本有排版空格，而 AI 学习 TSV 中仍保存无前导空格的 genuine 文本。

## 非目标

- ASCII 模式逐字直通时自动补空格；
- 根据宿主应用的光标周围文本补空格；
- 修改粘贴内容或已有正文；
- 在中文与数字之间添加空格；
- 改写候选内部的中英文、URL、邮箱或代码；
- 修改或重新构建 Squirrel。
