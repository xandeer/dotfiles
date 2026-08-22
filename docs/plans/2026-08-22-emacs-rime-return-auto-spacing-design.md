# Emacs Rime Return 自动空格设计

## 目标

让 Emacs Rime 与 Squirrel 共用同一套裸 Return 自动空格行为：

- `中文候选 + harness + Return -> 中文 harness`
- `harness + Return + 中文候选 -> harness 中文`

范围继续限制为中文模式、活跃 composition、完整 `[A-Za-z]+` 和裸 Return。数字、标点、URL、Shift+Return、Ctrl+J/L、普通直通输入、粘贴和光标移动不扩展。

## 根因

emacs-rime 的 `rime--return` 在 `rime-return-insert-raw` 为真时直接读取 `rime-lib-get-input` 并调用 `rime--commit`。它没有向 librime 发送 Return，因此 `rime.lua` 的 `select_character` Return 分支既无法为当前 Han→ASCII 提交前置空格，也无法把提交历史标为 `return_raw` 供下一次 ASCII→Han 使用。

隔离的 Emacs native session 已证明共享 librime 路径本身正确：直接发送 Rime Return 后，连续提交为 `中文`、`" harness"`、`" 中文"`。缺口只在 Emacs 的 Return 路由。

## 方案

在 `x-rime.el` 中增加 `rime-x-return` 命令，并把 `rime-active-mode-map` 的 `RET` 与 `<return>` 绑定到它。`rime-` 前缀让 emacs-rime 现有的 `rime--clear-state-before-unrelated-command` pre-command hook 将它识别为相关命令，避免在 Return 分发前清空 composition：

- 当 `rime-return-insert-raw` 为真时，把 `last-input-event` 规范化为 `return`，调用现有 `rime-send-keybinding`；它会向 librime 发送 `#xff0d`、读取合并后的 native commit，并复用现有显示、模式刷新和 AI lifecycle advice。
- 当 `rime-return-insert-raw` 为假时，继续调用 `rime--commit-preview`。
- Shift+Return 保留上游 `rime--shift-return` 绑定。

不覆盖或 advice 上游 `rime--return`，避免隐式全局修改；不在 Emacs 中重新实现 Han/ASCII 边界判断，避免与 `rime.lua` 漂移及误扩展到 host-buffer 编辑。

## 验证与部署

扩展现有 `tests/config/emacs-rime-module-regression.zsh`：

1. Elisp 集成测试先通过真实 emacs-rime pre-command hook 锁定 Return 分发前不会清空 composition，再验证 raw Return 必须经 `rime-send-keybinding` 且事件为 `return`，preview 分支保持原样，重复加载配置不会产生重复或错误绑定。
2. native module fixture 启用 `lua_processor@select_character` 与 `lua_filter@auto_space_filter`，验证真实提交序列为 `中文`、`" harness"`、`" 中文"`。
3. 继续运行集中 Rime/AI 回归、zsh 语法和 `git diff --check`。

实现只需重新加载当前 Emacs 中的 `x-rime.el`；不重编动态模块，也不改 Rime Lua 或 schema。真实编辑器输入仍由用户最终验收。
