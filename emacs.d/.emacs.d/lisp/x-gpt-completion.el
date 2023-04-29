;;; x-gpt-completion.el --- gpt completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'request)

(defcustom x/gpt-completion-4? 't
  "Variable controlling whether GPT-4 completion model is enabled.

If non-nil, GPT-4 completion will be enabled, otherwise it will be disabled."
  :type 'boolean
  :group 'x/gpt)

(defun x/gpt-completion-toggle-4 ()
  "Toggle the usage of GPT-4 completion in the current buffer."
  (interactive)
  ;; Toggle the value of `x/gpt-completion-4?' between t (true) and nil (false)
  (setq x/gpt-completion-4? (not x/gpt-completion-4?))
  ;; Display a message in the echo area showing the current state of
  ;; `x/gpt-completion-4?'
  (message "Use gpt-4? %s" x/gpt-completion-4?))

(defun x/gpt-completion (message callback)
  "Perform completion generation using OpenAI GPT with the given MESSAGE.

CALLBACK is a function with one argument, which is the JSON response.
It is called upon successful completion of the request.  If there is an
error, an error message will be logged and the CALLBACK will not be
invoked.

You must have stored your GPT API key as a password using
auth-source for the host 'openai.com' and user 'chatgpt'."
  (let* ((api-key (auth-source-pick-first-password
                   :host "openai.com"
                   :user "chatgpt"))
         (url "https://api.openai.com/v1/chat/completions")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " api-key))))
         (data `((model . ,(if x/gpt-completion-4? "gpt-4"
                             "gpt-3.5-turbo"))
                 (messages . (((role . "user") (content . ,message))))))
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

(defun x/gpt-completion-edit (instruction)
  "Call the GPT with the selected text or the region before the cursor.
INSTRUCTION is the guiding text."

  (interactive "sInstruction: ")

  ;; Check if the current buffer is not a minibuffer
  (if (not (minibufferp))
      (setq gpt-current-buffer (current-buffer)))
  (with-current-buffer
      gpt-current-buffer
    (let ((selected-text (if (use-region-p)
                             (buffer-substring-no-properties (region-beginning) (region-end))
                           (buffer-string)))
          (initial-point (point))
          (used-region (use-region-p))
          (region-beginning-saved (if (use-region-p) (region-beginning) nil))
          (region-end-saved (if (use-region-p) (region-end) nil))
          (point-max-saved (point-max))
          (point-min-saved (point-min))
          (original-buffer (current-buffer)))

      (x/gpt-completion
       (concat "### "
               instruction "\n\n"
               selected-text)
       (cl-function (lambda (&key data &allow-other-keys)
                      (let ((content (x/gpt-completion-parse-message data)))
                        ;; (message "Assistant's message: %s" content)
                        (with-current-buffer gpt-current-buffer ; Switch to the original buffer
                          (if used-region
                              (delete-region region-beginning-saved region-end-saved)
                            (delete-region point-min-saved point-max-saved))
                          (insert content)
                          (save-buffer)))))))))

(defun x/gpt-completion-prefix-for-code (instruction)
  "Construct a GPT code prompt prefix based on the current major mode.

INSTRUCTION is the message of the GPT completions."
  (concat "You're "
          (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
          "expert. "
          instruction))

(defun x/gpt-completion-edit-code (instruction)
  "Request a GPT completion for the given INSTRUCTION."
  (interactive "sInstruction: ")
  (let ((x/gpt-completion-4? t))
    (x/gpt-completion-edit
     (x/gpt-completion-prefix-for-code instruction))))

(defun x/gpt-completion-edit-text (instruction)
  "Request a GPT completion for the given INSTRUCTION."
  (interactive "sInstruction: ")
  (let ((x/gpt-completion-4? nil))
    (x/gpt-completion-edit
     (x/gpt-completion-prefix-for-code instruction))))

(transient-define-prefix x/gpt-completion-edit-group ()
  "Transient for gpt completion edit."
  ["Arguments"
   ("-f" "Use gpt-4?" "--gpt-4")]
  ["GPT edit"
   ["Text"
    ("c" "Chinese" (lambda () (interactive)
                     (message "--gpt-4? %s" (member "--gpt-4" (transient-args 'x/gpt-completion-edit-group)))
                     (x/gpt-completion-edit-text "Rewrite into simple Chinese")))
    ("e" "English" (lambda () (interactive)
                     (x/gpt-completion-edit-text "Rewrite into English, just give rewriten text")))
    ("g" "Git commit message" (lambda () (interactive)
                                (x/gpt-completion-edit-text "Rewrite into English, and make it shorter for git commit message, capialize the word after :")))]
   ["Code"
    ("d" "Doc String" (lambda () (interactive)
                        (x/gpt-completion-edit-code "Rewrite with documation, every line in 80 columns.")))
    ("i" "Implement comments" (lambda () (interactive)
                                (x/gpt-completion-edit-code "Implement the comments into code, and keep the comments")))]])

(x/define-keys
 global-map
 '(("H-g" x/gpt-completion-edit-group)))

(provide 'x-gpt-completion)
;;; x-gpt-completion.el ends here
