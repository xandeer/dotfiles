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

(provide 'x-chatgpt)
;;; x-chatgpt.el ends here
