;;; x-gpt.el --- gpt -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'request)

(defun openai-call-edit (model input instruction callback &optional temperature)
  "Call the OpenAI API with MODEL and INPUT, using INSTRUCTION to guide the model.
A CALLBACK function is called upon receiving a response from the API.
TEMPERATURE is an optional parameter that controls the randomness of the model's output."

  (let* ((api-key (auth-source-pick-first-password :host "openai.com" :user "chatgpt"))
         (url "https://api.openai.com/v1/edits")
         (headers `(("Content-Type" . "application/json")
                    ("Authorization" . ,(concat "Bearer " api-key))))
         (data `((model . ,model)
                 (input . ,input)
                 (instruction . ,instruction)
                 ,@(when temperature
                     (list (cons "temperature" temperature)))))
         (request-backend 'curl))
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

(defun gpt-elisp-edit-generic (model instruction &optional autosave)
  "Call the GPT-3 model with the selected text or the region before the cursor.
MODEL is the name of the GPT-3 model to use, and INSTRUCTION is the guiding
text.  If AUTOSAVE is non-nil, the buffer will be saved after processing."

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
      (openai-call-edit
       model selected-text instruction
       (cl-function (lambda (&key data &allow-other-keys)
                      (let ((choice (aref (cdr (assoc 'choices data)) 0)))
                        (with-current-buffer gpt-current-buffer ; Switch to the original buffer
                          (if used-region
                              (delete-region region-beginning-saved region-end-saved)
                            (delete-region point-min-saved point-max-saved))
                          (insert (concat (cdr (assoc 'text choice))))
                          (if autosave
                              (save-buffer))
                          ;; (goto-char (point-max))
                          )))) 0))))

(defun gpt-elisp-edit-code (instruction)
  (interactive "sInstruction: ")
  (gpt-elisp-edit-generic "code-davinci-edit-001" instruction t))

(defun gpt-elisp-edit-text (instruction)
  (interactive "sInstruction: ")
  (gpt-elisp-edit-generic "text-davinci-edit-001" instruction t))

(transient-define-prefix x/gpt-edit-group ()
  "Transient for gpt edit."
  ["GPT edit"
   ["Text"
    ("c" "Chinese" (lambda () (interactive)
                     (gpt-elisp-edit-text "Rewrite into simple Chinese")))
    ("e" "English" (lambda () (interactive)
                     (gpt-elisp-edit-text "Rewrite into English")))
    ("g" "Git commit message" (lambda () (interactive)
                                (gpt-elisp-edit-text "Rewrite into English, and make it shorter for git commit message, capialize the word after :")))]
   ["Code"
    ("d" "Doc String" (lambda () (interactive)
                        (gpt-elisp-edit-code (concat "This is "
                                                     (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
                                                     " code, write documentation"))))
    ("i" "Implement comments" (lambda () (interactive)
                        (gpt-elisp-edit-code (concat "You're "
                                                     (replace-regexp-in-string "-mode\\'" "" (symbol-name major-mode))
                                                     " expert, implement the comments into code"))))]])

;; (x/define-keys
;;  global-map
;;  '(("H-g" x/gpt-edit-group)))

(provide 'x-gpt)
;;; x-gpt.el ends here
