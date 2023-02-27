;;; x-chatgpt.el --- chatgpt -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; requires
;; https://github.com/mmabrouk/chatgpt-wrapper
;; pip install git+https://github.com/mmabrouk/chatgpt-wrapper
;; playwright install firefox

(defconst x--chatgpt-buffer-name "*ChatGPT*")

(defun x/chat-gpt ()
  "Wrapper for chatgpt-wrapper."
  (interactive)
  (when (equal (shell-command-to-string "pip list | grep chatGPT") "")
    (shell-command "pip install git+https://github.com/mmabrouk/chatgpt-wrapper")
    (message "chatgpt-wrapper installed through pip.")
    (chatgpt-login))
  (async-shell-command "chatgpt install" x--chatgpt-buffer-name)
  ;; (do-applescript "tell application id \"org.gnu.Emacs\" to activate")
  )

(defun x/chat-gpt-restart ()
  "Restart chatgpt."
  (interactive)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer x--chatgpt-buffer-name))
  (x/chat-gpt))

;;; gpt doctor
(defvar doctor-chatgpt-process nil)
(defvar doctor-chatgpt-replying nil)
(defvar doctor-chatgpt-ready nil)
(defvar doctor-chatgpt-recv-list nil)
(defvar doctor-chatgpt-send-list nil)

(defun doctor-chatgpt-filter (process output)
  "Filter for chatgpt process."
  (let ((buffer (process-buffer process)))
    (cond
     ((string-match "Logging in\.\.\." output)
      (setq doctor-chatgpt-ready t))
     ((not doctor-chatgpt-ready))
     ((equal output "Chatbot: \n")
      (setq doctor-chatgpt-replying t)
      (with-current-buffer "*doctor*" (read-only-mode 1)))
     (t
      (when-let* ((el (string-match "\n+You:\n+$" output)))
        (setq doctor-chatgpt-replying nil)
        (setq output (substring output 0 el)))
      (when (> (length output) 1) (push output doctor-chatgpt-recv-list))
      (with-current-buffer "*doctor*"
        (read-only-mode -1)
        (goto-char (point-max))
        (insert (if (eq (length doctor-chatgpt-recv-list) 1)
                    (string-replace output (string-trim (nth 0 doctor-chatgpt-recv-list)) "")
                  output))
        (if doctor-chatgpt-replying
            (read-only-mode 1)
          (if doctor-chatgpt-recv-list (insert "\n\n"))))))))

(defun doctor-chatgpt-start-process ()
  "Start a chat with ChatGPT."
  (when (and (processp doctor-chatgpt-process) (process-live-p doctor-chatgpt-process))
    (kill-process doctor-chatgpt-process))
  (setq doctor-chatgpt-recv-list nil)
  (setq doctor-chatgpt-send-list nil)

  (setq doctor-chatgpt-process
        (start-process "*doctor-chatgpt*" "*doctor-chatgpt*"
                       "python" "-m" "revChatGPT.V1"))
  (setq doctor-chatgpt-ready nil)
  (set-process-filter doctor-chatgpt-process 'doctor-chatgpt-filter))

(defun doctor-chatgpt-read-print ()
  "Top level loop."
  (interactive nil doctor-mode)
  (setq doctor-sent (save-excursion
                      (backward-sentence 1)
                      (buffer-substring-no-properties (point) (point-max))))
  (insert "\n")
  (push doctor-sent doctor-chatgpt-send-list)
  (setq doctor-chatgpt-replying t)
  (process-send-string doctor-chatgpt-process (concat doctor-sent "\n")))

(advice-add 'doctor :before #'doctor-chatgpt-start-process)
(advice-add 'doctor-read-print :override #'doctor-chatgpt-read-print)

;;; doctor with markdown
;; https://emacs-china.org/t/chatgpt-emacs-doctor/23773
;; require:
;; https://github.com/acheong08/ChatGPT
;; pip3 install revChatGPT

(defvar doctor-chatgpt-buffer-name "*doctor-chatgpt*")
(defvar doctor-chatgpt-process-buffer-name "*doctor-chatgpt-process*")
(defvar doctor-chatgpt-process nil)
(defvar doctor-chatgpt-replying nil)
(defvar doctor-chatgpt-ready nil)
(defvar doctor-chatgpt-recv-list nil)
(defvar doctor-chatgpt-send-list nil)

(defun doctor-chatgpt-filter (process output)
  "Filter for chatgpt process."
  ;; (message "doctor-chatgpt-filter: %s" output)
  (let ((buffer (process-buffer process)))
    (cond
     ((string-match "Logging in\.\.\." output)
      (setq doctor-chatgpt-ready t))
     ((not doctor-chatgpt-ready))
     ((equal output "Chatbot: \n")
      (setq doctor-chatgpt-replying t)
      (with-current-buffer doctor-chatgpt-buffer-name (read-only-mode 1)))
     (t
      (when-let* ((el (string-match "\n+You:\n+$" output)))
        (setq doctor-chatgpt-replying nil)
        (setq output (substring output 0 el)))
      (when (> (length output) 1) (push output doctor-chatgpt-recv-list))
      (with-current-buffer doctor-chatgpt-buffer-name
        (read-only-mode -1)
        (goto-char (point-max))
        ;; HACK: don't know why it will repeat the first send, so remove it
        (insert
         (if (eq (length doctor-chatgpt-recv-list) 1)
             (string-replace (string-trim (nth 0 doctor-chatgpt-send-list)) "" output)
           output))
        (if doctor-chatgpt-replying
            (read-only-mode 1)
          (if doctor-chatgpt-recv-list (insert "\n\n"))))))))

(defun doctor-chatgpt-start-process ()
  "Start a chat with ChatGPT."
  (when (and (processp doctor-chatgpt-process)
             (process-live-p doctor-chatgpt-process))
    (kill-process doctor-chatgpt-process))
  (setq doctor-chatgpt-recv-list nil)
  (setq doctor-chatgpt-send-list nil)

  (setq doctor-chatgpt-process
        (start-process
         doctor-chatgpt-process-buffer-name
         doctor-chatgpt-process-buffer-name
         "python" "-m" "revChatGPT.V1"))
  (setq doctor-chatgpt-ready nil)
  (set-process-sentinel doctor-chatgpt-process #'doctor-chatgpt-process-sentinel)
  (set-process-filter doctor-chatgpt-process #'doctor-chatgpt-filter))

(defun doctor-chatgpt-process-sentinel (process event)
  "Sentinel for chatgpt process.
PROCESS is the process that changed.
EVENT is a string describing the change."
  (setq doctor-chatgpt-ready nil)
  (message "%s end with the event '%s'" process event))

(defun doctor-chatgpt-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence.
ARG will be passed to `newline'."
  (interactive "*p" doctor-chatgpt-mode)
  (if (= (preceding-char) ?\n)
      (doctor-chatgpt-read-print)
    (newline arg)))

(defun doctor-chatgpt-read-print ()
  "Top level loop."
  (interactive nil doctor-chatgpt-mode)
  ;; send the sentence before point
  (let ((doctor-sent
         (save-excursion
           (backward-sentence 1)
           (buffer-substring-no-properties (point) (point-max)))))
    (insert "\n")
    (push doctor-sent doctor-chatgpt-send-list)
    (setq doctor-chatgpt-replying t)
    (process-send-string doctor-chatgpt-process (concat doctor-sent "\n"))))

(defvar-keymap doctor-chatgpt-mode-map
  "C-j" #'doctor-chatgpt-read-print
  "RET" #'doctor-chatgpt-ret-or-read)

(define-derived-mode doctor-chatgpt-mode markdown-mode "Doctor ChatGPT"
  "Major mode for running the ChatGPT.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the ChatGPT's answer."
  :interactive nil
  (setq-local word-wrap-by-category t)
  (visual-line-mode)
  (insert "Hi. I am the ChatGPT. Please ask me anything, each time you are finished talking, type RET twice.")
  (insert "\n\n"))

(defun doctor-chatgpt ()
  "Switch to `doctor-chatgpt-buffer' and start talking with ChatGPT."
  (interactive)
  (doctor-chatgpt-start-process)
  (switch-to-buffer doctor-chatgpt-buffer-name)
  (doctor-chatgpt-mode))

(provide 'x-chatgpt)
;;; x-chatgpt.el ends here
