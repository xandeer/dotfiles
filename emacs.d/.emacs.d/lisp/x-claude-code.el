;;; x-claude-code.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use '(claude-code . "stevemolitor/claude-code.el"))
(setenv "ANTHROPIC_BASE_URL" "https://anyrouter.top")
(setenv "ANTHROPIC_AUTH_TOKEN" (auth-source-pick-first-password
                                :host "anyrouter.top"
                                :user "xandeer"))

(provide 'x-claude-code)
;;; x-claude-code.el ends here
