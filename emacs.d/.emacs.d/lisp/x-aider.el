;;; x-aider.el --- aider -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; This file configures the Aider package with necessary environment variables
;;; and settings for use with OpenAI's API.

(x/package-use '(aider . "tninja/aider.el"))

(setenv "OPENAI_API_BASE" "https://models.inference.ai.azure.com")
(setenv "OPENAI_API_KEY" (auth-source-pick-first-password :host "ai.github.com" :user "kkevindu"))
(setq aider-args '("--model" "gpt-4o"))

(provide 'x-aider)
;;; x-aider.el ends here
