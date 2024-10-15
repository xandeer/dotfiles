;;; x-gpt-git.el --- gpt with git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'gptel)
(require 'x-gpt-code)

;; (setq x/gpt-commit-backend
;;       (gptel-make-gpt4all "GPT4All"
;;     :protocol "http"
;;     :host "localhost:4891"
;;     :models '("Meta-Llama-3-8B-Instruct.Q4_0.gguf")))
;; (setq x/gpt-commit-model "Meta-Llama-3-8B-Instruct.Q4_0.gguf")

(setq x/gpt-git-backend
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :stream t
        :models '("deepseek-coder-v2:16b")))

(setq x/gpt-git-model "deepseek-coder-v2:16b")

(setq x/gpt-git-backend x/gpt-gh)
(setq x/gpt-git-model "gpt-4o")

(setq x/gpt-git-max-tokens 16384)

(defun x/gpt-git-request (prompt callback)
  "Make a GPT request with Git changes.

PROMPT is a string that serves as the system prompt for the GPT request.
CALLBACK is a function that will be called with the result of the GPT request.

This function sets up the GPT request parameters using predefined variables:
- x/gpt-git-backend: The backend service to use for the GPT request.
- x/gpt-git-model: The model to use for the GPT request.
- x/gpt-git-max-tokens: The maximum number of tokens for the GPT request.

The function then calls gptel-request with the changes obtained from x/gpt-code--get-changes,
passing the PROMPT and CALLBACK to it.

Example usage:
  (x/gpt-git-request \"Describe the recent changes\" #'my-callback-function)

Dependencies:
- `gptel-request`: Function to make the GPT request.
- `x/gpt-code--get-changes`: Function to get the Git changes.

Arguments:
PROMPT -- The system prompt for the GPT request.
CALLBACK -- The function to call with the result of the GPT request."
  (let* ((gptel-backend x/gpt-git-backend)
         (gptel-model x/gpt-git-model)
         (gptel-max-tokens x/gpt-git-max-tokens))
    (gptel-request (x/gpt-code--get-changes)
      :system prompt
      :callback callback)))

(defun x/gpt-git-generate-commit-message ()
  "Generate a commit message using GPT and insert it into the commit buffer.

This function is interactive and can be called directly by the user.
If the current Git commit buffer does not already contain a commit message,
it makes a request to a GPT-based backend to generate a commit message.

The function uses `x/gpt-git-request` to make the GPT request with the prompt
defined in `x/gpt-prompt-code-generate-commit-message`. The generated commit
message is then inserted into the current buffer.

Example usage:
  M-x x/gpt-git-generate-commit-message

Dependencies:
- `git-commit-buffer-message`: Function to check if the commit buffer already has a message.
- `x/gpt-git-request`: Function to make the GPT request."
  (interactive)
  (unless (git-commit-buffer-message)
    (let ((buffer (current-buffer)))
      (x/gpt-git-request x/gpt-prompt-code-generate-commit-message
                         (lambda (commit-message info)
                           (if commit-message
                               (with-current-buffer buffer
                                 (insert commit-message))
                             (message "Error: %s" info)))))))

;; (add-hook 'git-commit-setup-hook 'x/gpt-git-generate-commit-message)

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
     (concat x/gpt-prompt-format-org x/gpt-prompt-code-review)
     (lambda (res info)
       (if res
           (with-current-buffer buffer
             (insert res)
             (pop-to-buffer buffer))
         (message "Error: %s" info))))))

(provide 'x-gpt-git)
;;; x-gpt-git.el ends here
