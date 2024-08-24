;;; x-gpt-commit.el --- gpt commit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'magit)
(require 'gptel)

(setq x/gpt-commit-backend
      (gptel-make-gpt4all "GPT4All"
    :protocol "http"
    :host "localhost:4891"
    :models '("Meta-Llama-3-8B-Instruct.Q4_0.gguf")))
(setq x/gpt-commit-model "Meta-Llama-3-8B-Instruct.Q4_0.gguf")

(defconst x/gpt-commit-system-prompt
  "The user provides the result of running `git diff --cached`. You suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **Fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **Feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `Fix:` and `Feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `Build:`, `Chore:`,
  `CI:`, `Docs:`, `Style:`, `Refactor:`, `Perf:`, `Test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `Feat(parser): add ability to parse arrays`.")

(defun x/gpt-commit-generate-message (callback)
  "Generate a commit message using GPT and pass it to the CALLBACK."
  (let* ((gptel-backend x/gpt-commit-backend)
         (gptel-model x/gpt-commit-model)
         (gptel-max-tokens 8000)
         (lines (magit-git-lines "diff" "--cached"))
         (changes (string-join lines "\n")))
    (gptel-request changes
      :system x/gpt-commit-system-prompt
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
      (x/gpt-commit-generate-message
       (lambda (commit-message info)
         (if commit-message
             (with-current-buffer buffer
               (insert commit-message))
           (message "Error: %s" info)))))))

(provide 'x-gpt-commit)
;;; x-gpt-commit.el ends here
