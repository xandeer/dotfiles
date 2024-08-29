;;; x-gpt-git.el --- gpt with git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'gptel)

;; (setq x/gpt-commit-backend
;;       (gptel-make-gpt4all "GPT4All"
;;     :protocol "http"
;;     :host "localhost:4891"
;;     :models '("Meta-Llama-3-8B-Instruct.Q4_0.gguf")))
;; (setq x/gpt-commit-model "Meta-Llama-3-8B-Instruct.Q4_0.gguf")

(setq x/gpt-commit-backend
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :stream t
        :models '("deepseek-coder-v2:16b")))

(setq x/gpt-commit-model "deepseek-coder-v2:16b")
(setq x/gpt-commit-max-tokens 80000)

(defconst x/gpt-commit-system-prompt
  "You are a professional developer assistant responsible for generating Git commit messages. Please create a concise, clear, and best-practice commit message based on the provided diff content. Ensure the commit message includes the following elements:

1. **Type** (e.g., Feat, Fix, Docs, Style, Refactor, Perf, Test, Chore, etc.)
2. **Brief Description**: Summarize the main purpose of this commit in one sentence.
3. **Detailed Description** (optional): If necessary, provide additional background or context explaining why these changes were made. They should be listed in bullet points.

Please follow this format for the commit message, without any other wrapping:

<type>: <brief description>

<detailed description>

Here is the diff content:")

(defun x/gpt-git--get-changes ()
  "Retrieve the cached Git diff and return it as a single string.

This function uses `magit-git-lines' to get the lines of the Git diff
for the cached changes (staged changes). It then joins these lines into
a single string with newline characters separating each line.

Returns:
  A string containing the cached Git diff."
  (string-join
   (magit-git-lines "diff" "--cached")
   "\n"))

(defun x/gpt-commit-request (prompt callback)
  "Send a commit request to GPT with the given PROMPT and handle the response with CALLBACK.

This function collects the staged changes from Git, formats them into a single string,
and sends them to the GPT system using gptel-request. The GPT system is configured
using predefined variables x/gpt-commit-backend, x/gpt-commit-model, and x/gpt-commit-max-tokens.

Arguments:
PROMPT -- The system prompt to send to the GPT system.
CALLBACK -- A function to handle the response from the GPT system.

Usage:
  (x/gpt-commit-request \"Review the following changes:\" #'my-callback-function)

Dependencies:
  - gptel-request: A function that sends a request to the GPT system.
  - magit-git-lines: A function from Magit to get the staged changes from Git.
  - Predefined variables: x/gpt-commit-backend, x/gpt-commit-model, x/gpt-commit-max-tokens.

Example:
  (x/gpt-commit-request \"Review the following changes:\" (lambda (res info) (message \"Response: %s\" res)))

This function is intended to be used programmatically."
  (let* ((gptel-backend x/gpt-commit-backend)
         (gptel-model x/gpt-commit-model)
         (gptel-max-tokens x/gpt-commit-max-tokens))
    (gptel-request (x/gpt-git--get-changes)
      :system prompt
      :callback callback)))

(defun x/gpt-commit-message ()
  "Automatically generate a conventional commit message using GPT-Commit.

This function is a hook intended to be added to `git-commit-setup-hook'.
When called, it analyzes the changes in the Git repository and generates
a conventional commit message using the GPT model.

The generated commit message follows the conventional commit format,
providing a structured description of the changes made in the commit.

Example usage:
  (require 'x/gpt-commit)
  (add-hook 'git-commit-setup-hook 'x/gpt-commit-message)"

  (interactive)
  (unless (git-commit-buffer-message)
    (let ((buffer (current-buffer)))
      (x/gpt-commit-request x/gpt-commit-system-prompt
                            (lambda (commit-message info)
                              (if commit-message
                                  (with-current-buffer buffer
                                    (insert commit-message))
                                (message "Error: %s" info)))))))

(defconst x/gpt-review-system-prompt "You are an experienced code reviewer tasked with reviewing code changes submitted by a developer. Please review the provided diff content and provide a detailed code review, addressing the following points:

1. **Overall Assessment**: Provide a high-level assessment of the changes, including the impact, complexity, and potential risks.

2. **Functional Changes**: Analyze the functional changes made in the code. Ensure they address the intended requirements and do not introduce unintended side effects.

3. **Code Quality**: Evaluate the code quality, considering factors such as readability, maintainability, and adherence to best practices and coding standards.

4. **Edge Cases and Error Handling**: Check if the code handles edge cases and potential errors appropriately.

5. **Performance and Scalability**: Assess the impact of the changes on performance and scalability, if applicable.

6. **Security Considerations**: Identify any potential security vulnerabilities or concerns introduced by the changes.

7. **Documentation and Comments**: Ensure the code is well-documented and commented, making it easier for other developers to understand and maintain.

8. **Suggested Improvements**: Provide constructive feedback and suggestions for improvement, focusing on areas that could be optimized or refactored.

Please provide your code review in a clear and structured format, addressing each point mentioned above. Use markdown formatting for better readability.

Here is the diff content:")

(defun x/gpt-review-changes ()
  "Review changes using GPT and display the response in a dedicated buffer.

This function creates or reuses a buffer named '*gpt-review*' to display the
response from the GPT system. It sends a request with a predefined prompt
`x/gpt-review-system-prompt` and handles the response asynchronously. If the
response is successful, it inserts the response into the '*gpt-review*' buffer
and pops to that buffer. If there is an error, it displays an error message
with the error information.

Usage:
  M-x x/gpt-review-changes

Dependencies:
  - `x/gpt-commit-request`: A function that sends a request to the GPT system.
  - `x/gpt-review-system-prompt`: A predefined prompt string for the GPT system.

Example:
  (x/gpt-review-changes)

This function is intended to be used interactively."
  (interactive)
  (let ((buffer (get-buffer-create "*gpt-review*")))
    (x/gpt-commit-request x/gpt-review-system-prompt
                          (lambda (res info)
                            (if res
                                (with-current-buffer buffer
                                  (insert res)
                                  (pop-to-buffer buffer))
                              (message "Error: %s" info))))))

(defun x/gpt-copilot-commit-message ()
  "Send to Copilot a commit message prompt followed by the git diff --cache code."
  (interactive)
  (unless (git-commit-buffer-message)
    (let* ((prompt x/gpt-commit-system-prompt)
           (changes (x/gpt-git--get-changes))
           (formatted-prompt (concat prompt "\n" changes))
           (current-buf (current-buffer)))

      (copilot-chat--ask formatted-prompt
                         (lambda (content)
                           (with-current-buffer current-buf
                             (insert (string-replace copilot-chat--magic "" content))))))))

(add-hook 'git-commit-setup-hook 'x/gpt-copilot-commit-message)

(defun x/gpt-copilot-review ()
  "Send to Copilot a review prompt followed by the  git diff --cache code."
  (interactive)
  (let* ((prompt x/gpt-review-system-prompt)
         (changes (x/gpt-git--get-changes))
         (formatted-prompt (concat prompt "\n" changes)))
    (with-current-buffer copilot-chat-prompt-buffer
      (erase-buffer)
      (insert formatted-prompt))
    (copilot-chat-prompt-send)))

(provide 'x-gpt-git)
;;; x-gpt-git.el ends here
