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

  (defun x/gpt-code--copilot-token ()
    "Get the copilot token."
    (auth-source-pick-first-password
     :host "copilot.github.com"
     :user "kkevindu"))

  (setf (copilot-chat-github-token copilot-chat--instance) (x/gpt-code--copilot-token))
  ;; (advice-add 'copilot-chat--get-cached-token :override #'x/gpt-code--copilot-token)

  ;; (defun x/gpt-code-copilot-chat--replace-src-block (content &optional buffer)
  ;;   (when (string= content copilot-chat--magic)
  ;;     (with-current-buffer (or buffer copilot-chat-buffer)
  ;;       (read-only-mode -1)
  ;;       (save-excursion
  ;;         (goto-char (point-min))
  ;;         (while (re-search-forward "^[[:space:]]*===\\(.+\\)$" nil t)
  ;;           (replace-match "#+begin_src \\1"))
  ;;         (goto-char (point-min))
  ;;         (while (re-search-forward "^[[:space:]]*===$" nil t)
  ;;           (replace-match "#+end_src\n")))
  ;;       (read-only-mode))))

  ;; (advice-add 'copilot-chat-prompt-cb :before #'x/gpt-code-copilot-chat--replace-src-block)
  ;; (advice-remove 'copilot-chat-prompt-cb  #'x/gpt-code-copilot-chat--replace-src-block)

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
           (formatted-prompt (concat x/gpt-prompt-code-generate-commit-message "\n" changes))
           (current-buf (current-buffer)))

      (copilot-chat--ask formatted-prompt
                         (lambda (content)
                           (with-current-buffer current-buf
                             (insert (string-replace copilot-chat--magic "" content))))))))
(add-hook 'git-commit-setup-hook 'x/gpt-code-generate-commit-message)

(defconst x/gpt-code--org-prompt "Use only Emacs org mode formatting in your answers
Make sure to include the programming language name at the start of the org mode code blocks.
This is an example of python code block in emacs org syntax:
#+begin_src python
def hello_world():
	print('Hello, World!')
#+end_src
Avoid wrapping the whole response in the block code.

Don't forget the most important rule when you are formatting your response: use emacs org syntax only.")

(defun x/gpt-code-review-changes ()
  "Send to Copilot a review prompt followed by the  git diff --cache code."
  (interactive)
  (let* ((changes (x/gpt-code--get-changes))
         (formatted-prompt (concat x/gpt-prompt-code-review "\n" changes)))
    (copilot-chat--prepare-buffers)
    (with-current-buffer copilot-chat-prompt-buffer
      (erase-buffer)
      (insert x/gpt-code--org-prompt)
      (insert formatted-prompt))
    (copilot-chat-prompt-send)))

(defun x/gpt-code-ask-region (prompt)
  "Send the selected region's code to Copilot with a given PROMPT.

This function extracts the code from the current region, prepares the
Copilot chat buffers, and sends the code along with the provided
PROMPT to Copilot for processing."
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (copilot-chat--prepare-buffers)
    (with-current-buffer copilot-chat-prompt-buffer
      (erase-buffer)
      (insert x/gpt-code--org-prompt)
      (insert (concat prompt "\n Code:\n" code)))
    (copilot-chat-prompt-send)))

(defun x/gpt-code-doc ()
  "Generate documentation for the selected region using copilot-chat."
  (interactive)
  (x/gpt-code-ask-region x/gpt-prompt-code-doc))

(defun x/gpt-code-optimize ()
  (interactive)
  (x/gpt-code-ask-region x/gpt-prompt-code-optimize))

(defun x/gpt-code-review ()
  (interactive)
  (x/gpt-code-ask-region x/gpt-prompt-code-review))

(defun x/gpt-code-explain ()
  (interactive)
  (x/gpt-code-ask-region x/gpt-prompt-code-explain))

(provide 'x-gpt-code)
;;; x-gpt-code.el ends here
