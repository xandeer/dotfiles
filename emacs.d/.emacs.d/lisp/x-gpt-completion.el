;;; x-gpt-completion.el --- gpt completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'request)
(require 'transient)

(defun x/gpt-get-api-key ()
  "Get the api key of openai."
  (auth-source-pick-first-password
   :host "openai.com"
   :user "chatgpt"))

(defconst x/gpt-buffer "*x/gpt*")

(defun x/gpt-completion (instruction message callback &optional gpt-4?)
  "Request GPT completion based on `INSTRUCTION' and `MESSAGE'.

This function sends a request to the OpenAI API to generate a
completion.  The completion is obtained by calling `CALLBACK' upon
a successful API response.  If `GPT-4?' is non-nil, it uses the
gpt-4 model; otherwise, it uses the gpt-3.5-turbo model."
  (let* ((api-key (x/gpt-get-api-key))
         (url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " api-key))))
         (data `((model . ,(if gpt-4? "gpt-4"
                             "gpt-3.5-turbo"))
                 (messages . (((role . "system") (content . ,instruction))
                              ((role . "user") (content . ,message))))))
         (request-backend 'curl))

    ;; Uncomment the following line to log the data of the request
    ;; (message "data: %S" (json-encode data))

    (request
      url
      :type "POST"
      :headers headers
      :data (json-encode data)
      :parser 'json-read
      :success callback
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error: %S" error-thrown))))))

(defun x/gpt-completion-parse-message (data)
  "Parse the completion message from DATA returned by OpenAI API.

DATA is an alist representing the decoded JSON response from
the GPT API.  The function extracts and returns the value
corresponding to the 'message' key of the first choice object."
  (let ((choice (aref (cdr (assoc 'choices data)) 0)))
    ;; (message "Assistant's choice: %s" choice)
    (cdr (caddr (assoc 'message choice)))))

(defun x/gpt-org-mode-instruction (instruction)
  "Modify INSTRUCTION to request Org-mode markup syntax."
  (concat instruction
          " Use org-mode markup syntax instead of markdown. Headings should start at 3rd level like '*** hello'."))

(defun x/gpt-insert-into-buffer (buffer content operation &optional instruction selected-text point-start point-end)
  "Insert CONTENT into BUFFER according to OPERATION.

BUFFER is the target buffer, CONTENT is the text to be inserted,
OPERATION is one of 'replace', 'append', or 'buffer'.  If
USED-REGION is non-nil, REGION-END-SAVED and POINT-MAX-SAVED are
used to determine the insertion point."
  (with-current-buffer buffer
    (pcase operation
      ('append (progn
                 (goto-char point-end)
                 (x/gpt-insert-separated-lines)))
      ('buffer (progn
                 (switch-to-buffer-other-window buffer)
                 (unless (eq major-mode 'org-mode)
                   (org-mode))
                 (goto-char (point-max))
                 (x/gpt-insert-separated-lines)
                 (insert (concat "** Instruction:\n" instruction "\n"))
                 (insert (concat "** Content:\n" selected-text "\n"))
                 (insert "** GPT:\n")))
      (_ (delete-region point-start point-end)))
    (insert content)))

(defun x/gpt-completion-edit (instruction &optional gpt-4? operation)
  "Request GPT model to generate completion based on `INSTRUCTION'.

The completion is applied to the selected text or the whole buffer
based on `OPERATION'.  If `GPT-4?' is non-nil, use GPT-4 model, otherwise
use GPT-3.5. The `OPERATION' can be `replace' (default), `append', `buffer'
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
           (selected-text (buffer-substring-no-properties point-start point-end))
           (original-buffer (current-buffer)))
      (when (eq operation 'buffer)
        (setq instruction (x/gpt-org-mode-instruction instruction)))
      (deactivate-mark)
      (x/gpt-completion
       instruction
       selected-text
       (cl-function (lambda (&key data &allow-other-keys)
                      (let* ((content (x/gpt-completion-parse-message data))
                             (op (or operation 'replace))
                             (buffer (if (eq op 'buffer) x/gpt-buffer
                                       original-buffer)))
                        (if (eq op 'message)
                            (kill-new (message content))
                          (x/gpt-insert-into-buffer buffer content op instruction selected-text point-start point-end)))))
       gpt-4?))))

(defun x/gpt-insert-separated-lines ()
  "Insert two line breaks at the end of the current line to separate the text."
  (goto-char (line-end-position))
  (insert "\n\n"))

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
    (x/gpt-completion-edit prefixed-instruction t chosen-operation)))

(defun x/gpt-completion-edit-text (instruction &optional gpt-4?)
  "Request a GPT completion for the given INSTRUCTION.

When `GPT-4?' is not nil, use \"gpt-4\", else \"gpt-3.5-turbo\"."
  (interactive "sInstruction: ")
  (x/gpt-completion-edit
   instruction
   (or gpt-4?
       (member "--gpt-4" (x/gpt-completion-args)))
   (pcase (x/gpt-completion-args)
     ((pred (lambda (args) (member "--append" args))) 'append)
     ((pred (lambda (args) (member "--buffer" args))) 'buffer)
     ((pred (lambda (args) (member "--message" args))) 'message)
     (_ 'replace))))

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
   ("-f" "Use gpt-4?" "--gpt-4")
   ("-a" "Instead of replacing, we can append." "--append")
   ("-b" "Write to x/gpt-buffer." "--buffer")
   ("-m" "Display in echo area." "--message")]
  ["GPT edit"
   ["Text"
    ("c" "Chinese"
     (lambda () (interactive)
       (x/gpt-completion-edit-text "You're an translator. Rewrite into simple Chinese")))
    ("e" "English"
     (lambda () (interactive)
       (x/gpt-completion-edit-text "Rewrite into English, just give rewriten text")))
    ("g" "Git commit message"
     (lambda () (interactive)
       (x/gpt-completion-edit-text "Rewrite into English, make it shorter for git commit message, capialize the word after :")))
    ("n" "Without instruction"
     (lambda () (interactive)
       (x/gpt-completion-edit-text "Let's think step by step.")))
    ("x" "With typing instruction" x/gpt)]
   ["Code"
    ("d" "Doc String"
     (lambda () (interactive)
       (x/gpt-completion-edit-code "Rewrite with concise and high quality docstring, every line must in 80 columns.")))
    ("i" "Implement comments"
     (lambda () (interactive)
       (x/gpt-completion-edit-code "Implement the comments into code, and keep the comments")))
    ("s" "Explain step by step"
     (lambda () (interactive)
       (x/gpt-completion-edit-code "Explain the code step by step using Chinese as comments.")))
    ("o" "Optimize"
     (lambda () (interactive)
       (x/gpt-completion-edit-code "Optimize the provided code for performance and maintainability.")))
    ("r" "Review"
     (lambda () (interactive)
       (x/gpt-completion-edit-code
        "Review code. 1. Provide improvement suggestions in Chinese for readability, efficiency, and best practices. 2. Identify issues."
        'buffer)))]])

(x/define-keys
 global-map
 '(("H-g" x/gpt-completion-edit-group)))

(provide 'x-gpt-completion)
;;; x-gpt-completion.el ends here
