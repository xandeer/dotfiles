;;; x-gpt.el --- gpt -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; gptel
(x/package-use '(gptel . "karthink/gptel"))
(require 'gptel)

(setq x/gpt--backend-local
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :stream t
        :models '("llama3.3:latest")))
(setq x/gpt--model-local "llama3.3:latest")

(setq x/gpt--gh-token (auth-source-pick-first-password :host "ai.github.com" :user "xandeer"))
(setq x/gpt--backend-gh (gptel-make-azure "github" ;Name, whatever you'd like
                 :host "models.inference.ai.azure.com"
                 :endpoint "/chat/completions" ;or equivalent
                 :stream t                     ;Enable streaming responses
                 :key #'x/gpt--gh-token          ;API key
                 :models '("gpt-4o" "gpt-4o-mini")))
(setq x/gpt--model-gh "gpt-4o")

(setq gptel-default-mode 'org-mode)
(add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
;; (setq gptel-max-tokens 3000)
(setq-default gptel-backend x/gpt--backend-gh)
(setq-default gptel-model x/gpt--model-gh)

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

(provide 'x-gpt)
;;; x-gpt.el ends here
