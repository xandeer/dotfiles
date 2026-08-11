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

采用 Lua filter 读取 Rime `commit_history`，并用 `ShadowCandidate` 为当前候选增加前导空格。

选择该方案的原因：

- 继续走 Rime 的正常候选选择与提交路径；
- genuine candidate 保持不变，便于保留用户词频、候选类型和现有 AI 学习语义；
- 不需要枚举空格、数字键、回车等所有选词按键；
- 不需要修改或重新构建 Squirrel 前端。

未采用的方案：

1. Lua processor 拦截选词键并调用 `engine:commit_text()`。该方案需要覆盖所有提交入口，容易绕过正常用户词典更新，并会与现有 processor 行为耦合。
2. 在 Squirrel 提交出口读取应用光标周围文本并统一格式化。该方案能覆盖更多编辑场景，但仅限 macOS，且会增加定制 Squirrel 补丁的维护成本，超出本功能范围。

## 组件与顺序

在 `rime/rime.lua` 中增加 `auto_space_filter`。在 `rime/double_pinyin_flypy.schema.yaml` 中把它放在现有 AI 过滤器之后、去重器之前：

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
- 自动空格包装发生在最终去重前；
- AI filter 的候选注入和排序逻辑不需要理解自动空格。

## 数据流

过滤器对每个候选执行以下步骤：

1. 只考虑 `candidate.start == 0` 的候选，避免把同一次 composition 的后续 segment 错当成跨提交边界。
2. 读取 `context.commit_history:latest_text()`。
3. 取历史文本最后一个 Unicode 字符和当前候选第一个 Unicode 字符。
4. 仅当边界为“汉字 -> ASCII 字母”或“ASCII 字母 -> 汉字”时需要空格。
5. 需要空格时，输出一个文本为 `" " .. candidate.text`、类型为 `auto_space` 的 `ShadowCandidate`；否则原样输出候选。
6. 不对候选内部做全局替换。

例如：

```text
历史末尾「个」 + 候选「harness」 -> 「 harness」
历史末尾「s」  + 候选「来」      -> 「 来」
```

汉字检测使用 Unicode 17.0 的下列 CJK 统一及兼容汉字范围，而不是把所有非 ASCII 字符都视为中文：

- `U+3400–U+4DBF`
- `U+4E00–U+9FFF`
- `U+F900–U+FAFF`
- `U+20000–U+2FA1F`
- `U+30000–U+3347F`

其中较宽的补充平面范围包含扩展 B–J、兼容汉字及其保留空位；只有实际出现在候选中的码点才会参与判断。英文检测只接受 ASCII 大小写字母。

## 边界规则

应插入空格：

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

URL、邮箱、`C++`、版本号等候选内部内容不做改写。如果整个候选以 ASCII 字母开头，仍可在它与前一个汉字之间增加一个空格。

## 失败策略

实现不维护跨应用私有状态，只使用 Rime 自己的提交历史。以下情况一律原样输出：

- 历史为空或已被清除；
- 历史或候选文本为空；
- UTF-8 字符无法可靠解析；
- 任一边界字符不属于明确的 Han 或 ASCII 字母类别；
- 候选不是 composition 的首段（`candidate.start ~= 0`）；
- 候选已经带有自动空格包装或前导空白。

Rime 会把无修饰的可打印 ASCII（包括手动空格）记录为 `thru`，并在无修饰退格或回车时清空提交历史。因此手动空格不会被重复添加，退格或回车后则因历史为空而不插入。

鼠标移动光标、粘贴、切换应用以及宿主应用自行修改文本对 librime-lua 不可见。发生这些操作后，过滤器无法确认光标前的真实字符，可能仍看到旧的 Rime 提交记录；这些场景明确不保证自动空格结果。实现不额外缓存或伪装成能够读取宿主正文，并在所有可检测的异常中原样输出。

## 现有功能兼容

### 以词定字

当前 `select_character` 使用 `context:get_commit_text()` 选择首字或尾字。自动空格候选会带一个前导空格，因此该 processor 必须从 `context.composition:toSegmentation()` 取得首 segment 的选中候选及其 `get_genuine()` 结果，而不能只查看当前活动候选。只有在首 segment 候选文本严格等于一个半角空格加 genuine 文本，且完整 commit text 也以这一个前缀开头时，才能确认它来自自动包装；不能仅凭文本以空格开头来猜测来源。

确认包装关系后：

- 从完整 commit text 开头移除且仅移除这个已证明的自动前缀，再选择首字或尾字；
- 提交时重新保留该前缀；
- `Ctrl+J/L` 不能把空格本身当作目标字符。

该追溯同样适用于首 segment 已确认、当前活动候选位于后续 segment 的 composition。若首 segment 无选中候选，或 display/genuine 关系不能证明存在自动包装，则保持原有以词定字逻辑。自动空格本身只包装 `candidate.start == 0` 的候选，因此不会在后续 segment 中产生新的自动前缀。

### AI 候选学习

现有学习路径对选中候选调用 `get_genuine()`。自动空格必须继续保留 genuine candidate，使持久化的候选类型和文本不含排版空格。自动空格只影响最终显示与上屏文本，不应进入 AI 学习 TSV。

### 用户词典与排序

普通候选仍通过原有选择路径提交。librime 会从 `UniquifiedCandidate` 中取第一项并解开一层 `ShadowCandidate` 来获得 genuine candidate，但 mocked Lua 测试和部署成功都不足以证明真实用户词典语义。验证必须在实际 Rime 会话中确认原始 comment、quality、排序和用户词频更新行为没有退化。

## 验证

### Lua 行为回归

新增 `tests/config/rime-auto-space-regression.lua`，由现有 `tests/config/rime-config-regression.zsh` 使用 Squirrel 自带 Lua 加载生产 `rime.lua` 后执行。该 harness 使用 mock 验证 Lua 判断和包装契约，不把结果当作真实 C++ ShadowCandidate 或用户词典行为证明。覆盖：

- 汉字到英文、英文到汉字；
- 数字、标点、已有空白、Emoji、空历史；
- 不重复包装；
- 仅处理 `candidate.start == 0`，后续 segment 原样输出；
- ShadowCandidate 的 genuine candidate 保持不变；
- `Ctrl+J/L` 对带自动前缀的候选正确提交；
- 两段 composition 中当前活动候选位于第二段时，`Ctrl+J/L` 仍从首 segment 识别并保留唯一自动前缀；
- mocked AI 候选及学习记录使用无前导空格的 genuine 文本。

### 配置回归

扩展 `tests/config/rime-config-regression.zsh`：

- 检查自动空格 harness 存在；
- 断言 filter 顺序严格为 `ai_candidate_filter -> auto_space_filter -> uniquifier`；
- 使用现有 bundled-Lua 运行器加载生产 Lua 与两个测试 harness。

### 静态和原生部署

- `zsh -n tests/config/rime-config-regression.zsh`
- `tests/config/rime-config-regression.zsh`
- Ruby/Psych YAML 解析
- `git diff --check`
- 临时 HOME 下执行 Rime 安装和原生 `rime_deployer --build`
- 检查部署和 Squirrel 日志中没有 Lua、编码或组件加载错误

原生部署只证明组件能够加载和编译配置，不证明 ShadowCandidate 解包、用户词典学习或真实提交行为；这些由下一节的实际会话验收承担。

### 真实输入验收

在临时部署的 Squirrel 中验证：

```text
一个 -> harness -> 来     = 一个 harness 来
Rime -> 输入法           = Rime 输入法
第 -> 3次                = 第3次
中文 -> 手动空格 -> Rime = 中文 Rime
中文，-> Rime            = 中文，Rime
```

另测 `Ctrl+J/L`、退格、回车和 AI 候选，并确认测试结果不宣称覆盖鼠标移动、粘贴或宿主应用改文。部署到当前用户的 Rime 目录不属于仓库实现，除非另行明确授权。

使用真实普通词典候选重复输入，确认候选排序与用户词频仍能正常学习，且用户词典没有产生带前导空格的词条。使用真实 AI 与 `ai_learned` 候选，确认上屏文本有排版空格，而 AI 学习 TSV 中仍保存无前导空格的 genuine 文本。

## 非目标

- ASCII 模式逐字直通时自动补空格；
- 根据宿主应用的光标周围文本补空格；
- 修改粘贴内容或已有正文；
- 在中文与数字之间添加空格；
- 改写候选内部的中英文、URL、邮箱或代码；
- 修改或重新构建 Squirrel。
