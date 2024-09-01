;;; x-gpt-code.el --- gpt for coding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'copilot-chat)
(require 'gptel-org)

;;; copilot chat
(with-eval-after-load 'copilot-chat
  (setq copilot-chat-frontend 'org)
  (copilot-chat-org-init)

  (defun x/gpt-code-copilot-chat--org-format-data (content type)
    (let ((data ""))
      (if (eq type 'prompt)
          (progn
            (setq copilot-chat--first-word-answer t)
            (setq x/gpt-code--in-src-block nil)
            (setq data (concat "* " (format-time-string "/%H:%M:%S/ ") (format "%s\n" content))))
        (when copilot-chat--first-word-answer
          (setq copilot-chat--first-word-answer nil)
          (setq data (concat "\n** " (format-time-string "/%H:%M:%S/ \n")))))

      (let* ((org (string-replace "**" "*" content))
             (org (string-replace "`" "=" org))
             (org (string-replace "####" "****" org))
             (org (string-replace "###" "***" org)))

        (setq data (concat data org)))
      data))

  (advice-add 'copilot-chat--org-format-data :override #'x/gpt-code-copilot-chat--org-format-data))

(defconst x/gpt-code-generate-commit-message-prompt
  "You are a professional developer assistant responsible for generating Git commit messages. Please create a concise, clear, and best-practice commit message based on the provided diff content. Ensure the commit message includes the following elements:

1. Type (e.g., Feat, Fix, Docs, Style, Refactor, Perf, Test, Chore, etc.) should be capitalized.
2. Brief Description: Summarize the main purpose of this commit in one sentence.
3. Detailed Description (optional): If necessary, provide additional background or context explaining why these changes were made. They should be listed in bullet points.


Please follow this format for the commit message, without any other wrapping like ```:
<Type>: <Brief Description>

<Detailed Description>

Not like below:
```
**Type**: Feat

**Brief Description**: Add file sharing functionality and improve logging in EntryAbility and FwPage.

**Detailed Description**:
```

Commit message examples:
```
Feat: Add a new feature

- Add a new feature to the project
- Improve the performance of the existing code
- Fix a bug in the existing code
```

```
Refactor: Improve the code quality

- Refactor the existing code to improve readability
- Remove the redundant code
- Optimize the code structure
```

Start with the diff:\n")

(defconst x/gpt-code-review-prompt "You are an experienced code reviewer tasked with reviewing code changes submitted by a developer. Please review the provided diff content and provide a detailed code review, addressing the following points:

1. *Overall Assessment*: Provide a high-level assessment of the changes, including the impact, complexity, and potential risks.

2. *Functional Changes*: Analyze the functional changes made in the code. Ensure they address the intended requirements and do not introduce unintended side effects.

3. *Code Quality*: Evaluate the code quality, considering factors such as readability, maintainability, and adherence to best practices and coding standards.

4. *Edge Cases and Error Handling*: Check if the code handles edge cases and potential errors appropriately.

5. *Performance and Scalability*: Assess the impact of the changes on performance and scalability, if applicable.

6. *Security Considerations*: Identify any potential security vulnerabilities or concerns introduced by the changes.

7. *Documentation and Comments*: Ensure the code is well-documented and commented, making it easier for other developers to understand and maintain.

8. *Suggested Improvements*: Provide constructive feedback and suggestions for improvement, focusing on areas that could be optimized or refactored.

Please provide your code review in a clear and structured format, addressing each point mentioned above. Use markdown formatting for better readability.

Here is the diff content:")

(defun x/gpt-code--get-changes ()
  "Retrieve the cached Git diff and return it as a single string.

This function uses `magit-git-lines' to get the lines of the Git diff
for the cached changes (staged changes). It then joins these lines into
a single string with newline characters separating each line.

Returns:
  A string containing the cached Git diff."
  (string-join
   (magit-git-lines "diff" "--cached")
   "\n"))

(defun x/gpt-code-generate-commit-message ()
  "Send to Copilot a commit message prompt followed by the git diff --cache code."
  (interactive)
  (unless (git-commit-buffer-message)
    (let* ((changes (x/gpt-code--get-changes))
           (formatted-prompt (concat x/gpt-code-generate-commit-message-prompt "\n" changes))
           (current-buf (current-buffer)))

      (copilot-chat--ask formatted-prompt
                         (lambda (content)
                           (with-current-buffer current-buf
                             (insert (string-replace copilot-chat--magic "" content))))))))
(add-hook 'git-commit-setup-hook 'x/gpt-code-generate-commit-message)

(defun x/gpt-code-review ()
  "Send to Copilot a review prompt followed by the  git diff --cache code."
  (interactive)
  (let* ((changes (x/gpt-code--get-changes))
         (formatted-prompt (concat x/gpt-code-review-prompt "\n" changes)))
    (copilot-chat--prepare-buffers)
    (with-current-buffer copilot-chat-prompt-buffer
      (erase-buffer)
      (insert formatted-prompt))
    (copilot-chat-prompt-send)))

(provide 'x-gpt-code)
;;; x-gpt-code.el ends here
