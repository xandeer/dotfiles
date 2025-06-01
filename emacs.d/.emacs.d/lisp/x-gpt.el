;;; x-gpt.el --- gpt -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; gptel
(x/package-use '(gptel . "karthink/gptel"))
(require 'gptel)

(setq x/gpt--local-models '(qwen3:latest devstral:latest))

(setq x/gpt--backend-local
      (gptel-make-ollama "Ollama"
        :host "localhost:11434"
        :stream t
        :models x/gpt--local-models))

;; github
(setq x/gpt--gh-models '(openai/gpt-4.1 openai/o4-mini xai/grok-3 meta/Llama-4-Maverick-17B-128E-Instruct-FP8 deepseek/DeepSeek-R1))
(setq x/gpt--gh-token (auth-source-pick-first-password :host "ai.github.com" :user "xandeer"))
(setq x/gpt--backend-gh (gptel-make-azure "github" ;Name, whatever you'd like
                          :host "models.github.ai"
                          :endpoint "/inference/chat/completions" ;or equivalent
                          :stream t     ;Enable streaming responses
                          :key #'x/gpt--gh-token ;API key
                          :header `(("Authorization" . ,(concat "Bearer " x/gpt--gh-token)))
                          :models x/gpt--gh-models))

(setq x/gpt-model 'openai/gpt-4.1)
(setq x/gpt-backend x/gpt--backend-gh)

(defun x/gpt--match-backend ()
  "Match backend for `x/gpt-model'."
  (setq x/gpt-backend
        (if (memq x/gpt-model x/gpt--local-models)
            x/gpt--backend-local
          x/gpt--backend-gh))
  (setq-default gptel-backend x/gpt-backend))

;; deepseek
(setq x/gpt--ds-token (auth-source-pick-first-password :host "deepseek" :user "ds"))
;; OPTIONAL configuration
(setq x/gpt--model-ds "deepseek-reasoner"
      x/gpt--backend-ds
      (gptel-make-openai "DeepSeek"     ;Any name you want
        :host "api.deepseek.com"
        :endpoint "/chat/completions"
        :stream t
        :key x/gpt--ds-token          ;can be a function that returns the key
        :models '(deepseek-chat deepseek-reasoner)))

(setq gptel-default-mode 'org-mode)
(add-hook 'gptel-post-stream-hook #'gptel-auto-scroll)
;; (setq gptel-max-tokens 3000)
(setq-default gptel-backend x/gpt-backend)
(setq-default gptel-model x/gpt-model)
;; (setq gptel-log-level 'debug)

;; (setq-default gptel-backend x/gpt--backend-ds)
;; (setq-default gptel-model x/gpt--model-ds)
(setq gptel-include-reasoning nil)

(defvar x/gpt-model-history
  (mapcar #'symbol-name
          (append '() x/gpt--local-models x/gpt--gh-models))
  "The history list for ai models.")

(defun x/gpt-switch-model ()
  "Switch gpt model."
  (interactive)
  (let ((model (completing-read "Model: "
                                x/gpt-model-history
                                nil nil nil
                                'x/gpt-model-history)))
    (setq x/gpt-model (intern model))
    (x/gpt--match-backend)
    (setq-default gptel-model x/gpt-model)
    (message (format "Switch model to %s" model))))

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
