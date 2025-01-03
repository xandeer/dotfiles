;;; x-chatgpt.el --- chatgpt -*- lexical-binding: t -*-
;;; Commentary:

;; Before using this package, you need to install the revChatGPT
;; and set the API key with the `auth-source' package.
;; You can use pip to install the revChatGPT:
;;
;; pip3 install --upgrade revChatGPT
;;
;; And you can find more details in the `doctor-chatgpt-api-token'.
;; After that, you can use `doctor-chatgpt' to ask ChatGPT.

;;; Links:

;; https://emacs-china.org/t/chatgpt-emacs-doctor/23773
;; https://github.com/acheong08/ChatGPT
;; https://chat.openai.com/api/auth/session

;;; Code:

;;; gptel
(x/package-use '(gptel . "karthink/gptel"))
(require 'gptel)
(setq gptel-api-key (auth-source-pick-first-password :host "openai.com" :user "chatgpt"))
(setq gptel-default-mode 'org-mode)
(setq-default gptel-model "gpt-4o")
(setq gptel-max-tokens 3000)

(add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)

(setq x/gh-ai-token (auth-source-pick-first-password :host "ai.github.com" :user "xandeer"))

(setq x/gpt-gh (gptel-make-azure "github" ;Name, whatever you'd like
                 :protocol "https"        ;Optional -- https is the default
                 :host "models.inference.ai.azure.com"
                 :endpoint "/chat/completions" ;or equivalent
                 :stream t                     ;Enable streaming responses
                 :key #'x/gh-ai-token          ;API key
                 :models '("gpt-4o" "gpt-4o-mini")))

(defun x/gpt-from-anywhere ()
  "Use `gptel' to generate text from anywhere."
  (interactive)
  (let* ((screen-width (display-pixel-width))
         (screen-height (display-pixel-height))
         (frame-width (/ screen-width 3))
         (frame-height screen-height)
         (frame-left (- screen-width frame-width))
         (frame-top 0)
         (chat-frame (make-frame `((window-system . ns) ;;change this if you are not on macOS. For example you can use "x" instead of "ns" for x systems. Refer to make-frame documentation for more details
                                   (top . ,frame-top)
                                   (left . ,frame-left)
                                   (width . (text-pixels . ,frame-width))
                                   (height . (text-pixels . ,frame-height))
                                   (minibuffer . t)))))
    (select-frame chat-frame))
  (add-hook 'gptel-post-response-hook (lambda () (goto-char (point-max))))
  (gptel "#gpt anywhere#" gptel-api-key nil)
  (switch-to-buffer "#gpt anywhere#")
  (delete-other-windows))

(provide 'x-chatgpt)
;;; x-chatgpt.el ends here
