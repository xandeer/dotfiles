;;; x-gpt-completion.el --- gpt completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'transient)
(require 'x-gpt)
(require 'x-gpt-prompts)

(defun x/gpt-buffer-get ()
  "Return the buffer of x-gpt-completion.org in notes."
  (find-file-noselect (x/expand-note "x-gpt-completion.org")))

(defun x/gpt-completion (instruction message callback)
  "Send a completion request to a GPT model with step-by-step reasoning.

`INSTRUCTION': A string containing the system-level instructions that
             guide the behavior of the GPT model.
             Example: \"Explain this code.\"

`MESSAGE': A string containing the user-level input or query to be processed by the model.

`CALLBACK': A function to handle the response from the GPT model.

This function sends a request to the GPT model, formatting the system-level
instruction by appending \"Let's think step by step.\" to encourage the
model to provide reasoning or explanations in response."
  (let ((gptel-model 'gpt-4o))          ; use gpt-4o before https://github.com/karthink/gptel/pull/592 merged
    (gptel-request message
      :system (concat instruction " Let's think step by step.")
      :callback callback)))

(defun x/gpt-insert-into-buffer (buffer content operation &optional instruction selected-text point-start point-end)
  "Insert CONTENT into BUFFER according to OPERATION.

BUFFER is the target buffer, CONTENT is the text to be inserted,
OPERATION is one of 'replace', 'append', or 'buffer'.  If
USED-REGION is non-nil, REGION-END-SAVED and POINT-MAX-SAVED are
used to determine the insertion point."
  (with-current-buffer buffer
    (pcase operation
      ('append (save-excursion
                 (goto-char point-end)
                 (x/gpt-insert-separated-line)))
      ('buffer (progn
                 (switch-to-buffer-other-window buffer)
                 (unless (eq major-mode 'org-mode)
                   (org-mode))
                 (goto-char (point-max))
                 (x/gpt-insert-separated-line)
                 (insert (concat "** Instruction:\n" instruction "\n"))
                 (insert (concat "** Content:\n" selected-text "\n"))
                 (insert "** GPT:\n")))
      (_ (save-excursion (delete-region point-start point-end))))
    (if (equal operation 'buffer)
        (insert (gptel--convert-markdown->org content))
      (save-excursion
        (if (equal operation 'append)
            (goto-char (1+ point-end))
          (goto-char point-start))
        (insert (gptel--convert-markdown->org content))))))

(defun x/gpt-completion-edit (instruction &optional operation)
  "Request GPT model to generate completion based on `INSTRUCTION'.

The completion is applied to the selected text or the whole buffer
based on `OPERATION'. The `OPERATION' can be `replace' (default), `append', `buffer'
or anyother symbol.  `replace' replaces the text with the completion,
`append' appends the completion, `buffer' appends to `x/gpt-buffer',
and other symbols display the completion as a message."
  (interactive "sInstruction: ")

  ;; Check if the current buffer is not a minibuffer
  (if (not (minibufferp))
      (setq x/gpt-completion-edit-buffer (current-buffer)))
  (with-current-buffer
      x/gpt-completion-edit-buffer
    (let* ((point-start (if (use-region-p) (region-beginning) (point-min)))
           (point-end (if (use-region-p) (region-end) (point-max)))
           (selected-text (x/text-normalize (buffer-substring-no-properties point-start point-end)))
           (original-buffer (current-buffer)))
      (deactivate-mark)
      (x/gpt-completion
       instruction
       selected-text
       (lambda (response info)
         (if response
             (let* ((op (or operation 'replace))
                    (buffer (if (eq op 'buffer) (x/gpt-buffer-get)
                              original-buffer)))
               (if (eq op 'message)
                   (kill-new response)
                 (x/gpt-insert-into-buffer buffer response op instruction selected-text point-start point-end)))
           (message "Error: %S" info)))))))

(defun x/gpt-insert-separated-line ()
  "Insert two line breaks at the end of the current line to separate the text."
  (unless (eolp)
    (goto-char (line-end-position)))
  (insert "\n"))

(defun x/gpt-completion-prefix-for-code (instruction)
  "Construct a GPT code prompt prefix based on the current major mode.

INSTRUCTION is the message of the GPT completions."
  (concat "You're "
          (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
          " expert. "
          instruction))

(defun x/gpt-completion-edit-code (instruction &optional operation)
  "Use GPT completion result with to edit the selected code.

`INSTRUCTION' is a string representing the task to be completed.
`OPERATION' determines how the result will be inserted into the buffer.
It accepts two values:
  'buffer: append the result in `x/gpt-buffer'
  'replace: replace current region with result

If no OPERATION is provided, it defaults to 'replace, if '--buffer' is
in the list returned by `x/gpt-completion-args', otherwise 'replace."
  (interactive "sInstruction: ")
  (let ((prefixed-instruction (x/gpt-completion-prefix-for-code instruction))
        (chosen-operation
         (or operation
             (if (member "--buffer" (x/gpt-completion-args))
                 'buffer
               'replace))))
    (x/gpt-completion-edit prefixed-instruction chosen-operation)))

(defun x/gpt-completion-edit-text (instruction &optional operation)
  "Request a GPT completion for the given INSTRUCTION."
  (interactive "sInstruction: ")
  (x/gpt-completion-edit
   instruction
   (or operation
       (pcase (x/gpt-completion-args)
         ((pred (lambda (args) (member "--append" args))) 'append)
         ((pred (lambda (args) (member "--buffer" args))) 'buffer)
         ((pred (lambda (args) (member "--message" args))) 'message)
         (_ 'replace)))))

(defun x/gpt-completion-args ()
  "Return a list of command-line arguments for GPT completion.
These arguments are parsed from the transient arguments of 'x/gpt-completion-edit-group'."
  (transient-args 'x/gpt-completion-edit-group))

(defvar x/gpt-instruction-history nil)

(defun x/gpt ()
  "Prompt the user for an instruction and call `x/gpt-completion-edit-text'.

The user is prompted with completion based on `x/gpt-instruction-history'.
The selected or entered instruction is passed to the function
`x/gpt-completion-edit-text'."
  (interactive)
  (let ((instruction
         (completing-read "Instruction: "
                          x/gpt-instruction-history
                          nil nil nil
                          'x/gpt-instruction-history)))
    (x/gpt-completion-edit-text instruction)))

(transient-define-prefix x/gpt-completion-edit-group ()
  "Transient for gpt completion edit."
  ["Arguments"
   ("-a" "Instead of replacing, we can append." "--append")
   ("-b" "Write to x/gpt-buffer." "--buffer")
   ("-m" "Display in echo area." "--message")]
  ["GPT edit"
   ["Text"
    ("ta" "Alternate"
     (lambda () (interactive)
       (x/gpt-completion-edit-text x/gpt-prompt-text-alter 'append)))
    ("tc" "Chinese"
     (lambda () (interactive)
       (x/gpt-completion-edit-text x/gpt-prompt-text-to-zh)))
    ("te" "English"
     (lambda () (interactive)
       (x/gpt-completion-edit-text x/gpt-prompt-text-to-en)))
    ("ts" "Summarize"
     (lambda () (interactive)
       (x/gpt-completion-edit-text x/gpt-prompt-text-summarize 'buffer)))
    ("tn" "Without instruction"
     (lambda () (interactive)
       (x/gpt-completion-edit-text "Let's think step by step.")))
    ("tx" "With typing instruction" x/gpt)]
   ["English"
    ("ee" "Example"
     (lambda () (interactive)
       (x/gpt-completion-edit-text x/gpt-prompt-english-word-example 'append)))
    ("es" "Explain sentence structure"
     (lambda () (interactive)
       (x/gpt-completion-edit-text x/gpt-prompt-english-explain-sentence 'append)))]
   ["Copilot Chat"
    ("cd" "Doc" (lambda () (interactive)
                  (x/gpt-completion-edit-code x/gpt-prompt-code-doc 'buffer)))
    ("ci" "Implement comments"
     (lambda () (interactive)
       (x/gpt-completion-edit-code "Implement the comments into code, and keep the comments")))
    ;; ("s" "Explain step by step"
    ;;  (lambda () (interactive)
    ;;    (x/gpt-completion-edit-code "Let's think step by step. Explain the code step by step using Chinese as comments." 'buffer)))
    ("ce" "Explain step by step" (lambda () (interactive)
                                   (x/gpt-completion-edit-code x/gpt-prompt-code-explain 'buffer)))
    ("co" "Optimize" (lambda () (interactive)
                       (x/gpt-completion-edit-code x/gpt-prompt-code-optimize 'buffer)))
    ("cr" "Review" (lambda () (interactive)
                     (x/gpt-completion-edit-code x/gpt-prompt-code-review 'buffer)))
    ("cf" "Fix" (lambda () (interactive)
                  (x/gpt-completion-edit-code x/gpt-prompt-code-fix 'buffer)))
    ("cp" "Custom Prompt" (lambda () (interactive)
                            (x/gpt-completion-edit-code (completing-read "Instruction: "
                                                                         x/gpt-instruction-history
                                                                         nil nil nil
                                                                         'x/gpt-instruction-history) 'buffer)))]])

(x/define-keys
 global-map
 '(("H-g" x/gpt-completion-edit-group)))

(provide 'x-gpt-completion)
;;; x-gpt-completion.el ends here
