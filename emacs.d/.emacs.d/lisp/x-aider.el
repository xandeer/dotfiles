;;; x-aider.el --- aider -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; This file configures the Aider package with necessary environment variables
;;; and settings for use with OpenAI's API.

(x/package-use '(aider . "tninja/aider.el"))

(setq openai-api-base "https://models.inference.ai.azure.com")
(setq openai-api-key (auth-source-pick-first-password :host "ai.github.com" :user "kkevindu"))
(setenv "OPENAI_API_BASE" openai-api-base)
(setenv "OPENAI_API_KEY" openai-api-key)
(setq aider-args '("--model" "gpt-4o"))

(provide 'x-aider)
;;; x-aider.el ends here
