;;; x-claude-code.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use '(claude-code-ide . "manzaltu/claude-code-ide.el"))
(x/package-use 'vterm)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "M-i") #'toggle-input-method))
(claude-code-ide-emacs-tools-setup)
(setq claude-code-ide-terminal-backend 'vterm)

(setenv "ANTHROPIC_BASE_URL" "https://anyrouter.top")
(setenv "ANTHROPIC_AUTH_TOKEN" (auth-source-pick-first-password
                                :host "anyrouter.top"
                                :user "xandeer"))

(defun switch-to-gc ()
  "切换到 antigravity claude（gc）。"
  (interactive)
  ;; 设置环境变量
  (setenv "ANTHROPIC_BASE_URL" "http://localhost:8989/antigravity")
  (setenv "ANTHROPIC_AUTH_TOKEN" "test")
  (setenv "ANTHROPIC_MODEL" "opus")
  (setenv "ANTHROPIC_DEFAULT_MODEL" "opus")
  (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" "claude-opus-4-5-thinking")
  (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL" "gemini-3-flash")
  ;; 输出提示
  (message "Switched to antigravity claude"))

(defun switch-to-gg ()
  "切换到 antigravity gemini（gg）。"
  (interactive)
  ;; 设置环境变量
  (setenv "ANTHROPIC_BASE_URL" "http://localhost:8989/antigravity")
  (setenv "ANTHROPIC_AUTH_TOKEN" "test")
  (setenv "ANTHROPIC_MODEL" "opus")
  (setenv "ANTHROPIC_DEFAULT_MODEL" "opus")
  (setenv "ANTHROPIC_DEFAULT_OPUS_MODEL" "gemini-3-pro-high")
  (setenv "ANTHROPIC_DEFAULT_HAIKU_MODEL" "gemini-3-flash")
  ;; 输出提示
  (message "Switched to antigravity gemini"))


(provide 'x-claude-code)
;;; x-claude-code.el ends here
