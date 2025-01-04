;;; x-gpt-git.el --- gpt with git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'gptel)
(require 'x-chatgpt)

;; (setq x/gpt-commit-backend
;;       (gptel-make-gpt4all "GPT4All"
;;     :protocol "http"
;;     :host "localhost:4891"
;;     :models '("Meta-Llama-3-8B-Instruct.Q4_0.gguf")))
;; (setq x/gpt-commit-model "Meta-Llama-3-8B-Instruct.Q4_0.gguf")

;; (setq x/gpt-git-backend
;;       (gptel-make-ollama "Ollama"
;;         :host "localhost:11434"
;;         :stream t
;;         :models '("deepseek-coder-v2:16b")))

;; (setq x/gpt-git-model "deepseek-coder-v2:16b")

(setq x/gpt-git-backend x/gpt-local-backend)
(setq x/gpt-git-model x/gpt-local-model)

(setq x/gpt-git-max-tokens 16384)

(defun x/gpt-git-request (prompt buffer position)
  "Make a GPT request for Git-related operations.

This function sets up the GPT backend, model, and token limit for Git operations,
then makes a request to the GPT model using the current Git changes.

Arguments:
PROMPT: The system prompt to be used for the GPT request.
BUFFER: The buffer where the response will be inserted.
POSITION: The position in the buffer where the response should be inserted.

The function uses `x/gpt-git-backend`, `x/gpt-git-model`, and `x/gpt-git-max-tokens`
as the GPT backend, model, and maximum token limit respectively.

It calls `x/gpt-code--get-changes` to retrieve the current Git changes and uses
them as the content for the GPT request.

Returns:
The result of the `gptel-request` call, which is typically the response from the GPT model."
  (let* ((gptel-backend x/gpt-git-backend)
         (gptel-model x/gpt-git-model)
         (gptel-max-tokens x/gpt-git-max-tokens))
    (gptel-request (x/gpt-code--get-changes)
      :system prompt
      :stream t
      :buffer buffer
      :position position)))

(defun x/gpt-git-generate-commit-message ()
  "Generate a commit message using GPT.

This function is interactive and can be called directly by the user.
It checks if there's already a commit message in the buffer. If not,
it uses GPT to generate a commit message based on the current Git changes.

The function uses `x/gpt-git-request` to make a request to the GPT model,
passing the prompt defined in `x/gpt-prompt-code-generate-commit-message`.
The generated message is then inserted at the beginning of the current buffer.

This function is particularly useful when added to `git-commit-setup-hook`
to automatically generate commit messages."
  (interactive)
  (unless (git-commit-buffer-message)
    (x/gpt-git-request x/gpt-prompt-code-generate-commit-message
                       (current-buffer)
                       (point-min))))

(add-hook 'git-commit-setup-hook 'x/gpt-git-generate-commit-message)

(defun x/gpt-git-review-changes ()
  "Review Git changes using GPT and display the review in a dedicated buffer.

This function is interactive and can be called directly by the user.
It makes a request to a GPT-based backend to review the Git changes using the prompt
defined in `x/gpt-prompt-code-review`. The review result is then inserted into a buffer
named `*gpt-review*` and displayed to the user.

Example usage:
  M-x x/gpt-git-review-changes

Dependencies:
- `x/gpt-git-request`: Function to make the GPT request.
- `x/gpt-code-review-prompt`: Prompt used for the GPT request."
  (interactive)
  (let ((buffer (get-buffer-create "*gpt-review*")))
    (x/gpt-git-request
     x/gpt-prompt-code-review
     buffer
     (with-current-buffer buffer
       (org-mode)
       (pop-to-buffer buffer)
       (point-max)))))

(provide 'x-gpt-git)
;;; x-gpt-git.el ends here
