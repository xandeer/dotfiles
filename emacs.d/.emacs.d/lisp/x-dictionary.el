;;; x-dictionary.el --- x-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; sdcv
(autoload #'sdcv-search-pointer "sdcv" nil t)
(autoload #'sdcv-search-input "sdcv" nil t)

(setq sdcv-program (executable-find "sdcv"))
(setq sdcv-say-word-p t)
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict"))

(with-eval-after-load 'sdcv
  ;; face
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")

  (defun x-sdcv--translate-br (s)
    (s-replace "<br>" "\n" s))
  (advice-add 'sdcv-translate-result :filter-return #'x-sdcv--translate-br)

  ;; say
  (defun x/sdcv-say (word)
    (when word
      (azure-tts-play word
                      (if (jieba-chinese-word? word) azure-tts-zh-voice-name
                        azure-tts-en-voice-name)
                      azure-tts-default-rate
                      azure-tts-default-pitch)))

  (advice-add 'sdcv-say-word :override #'x/sdcv-say)
  ;; (advice-remove 'sdcv-say-word 'x/sdcv-say)

  (defvar x/sdcv-say-word-p t)

  (defun x-sdcv--say-word (&optional word)
    (when (and x/sdcv-say-word-p
               word
               (not (jieba-chinese-word? word)))
      (osx-lib-say word)))

  ;; (setq osx-lib-say-voice "Ting-Ting")

  ;; (advice-add 'sdcv-search-detail :after #'x-sdcv--say-word)

  (define-key sdcv-mode-map (kbd "s")
              (lambda ()
                (interactive)
                ;; (osx-lib-say sdcv-current-translate-object)
                (sdcv-say-word sdcv-current-translate-object))))

;;; translate
(autoload #'facemenu-add-face "facemenu" nil t)
(autoload #'gts-translator "go-translate" nil t)

(setq gts-translate-list '(("zh" "en")))

(defun x/deepl-key ()
  "Get the api token from th authinfo."
  (auth-source-pick-first-password :host "deepl.com" :user "deepl"))

(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       ;; :engines (list (gts-google-engine) (gts-google-rpc-engine))
       ;; :engines `(,(gts-google-engine))
       ;; :engines `(,(gts-bing-engine))
       :engines `(,(gts-deepl-engine :auth-key (x/deepl-key) :pro nil))
       :render (gts-buffer-render)))

;;; immersive-translate
(setq immersive-translate-backend 'chatgpt)
(add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
(add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)

;;; xwidget
(defvar x/translate-history nil
  "History of translate.")

(defvar x/translate-to-zh-url
  ;; "https://translate.google.com/?sl=auto&tl=zh-CN&op=translate&text=%s"
  "https://www.deepl.com/translator#en/zh/%s"
  )

(defvar x/translate-to-en-url
  "https://translate.google.com/?sl=auto&tl=en&op=translate&text=%s"
  ;; "https://www.deepl.com/translator#zh/en/%s"
  )

(defun x/translate-to-zh ()
  "Translate TEXT to zh."
  (interactive)
  (let ((text (completing-read "2zh: "
                               x/translate-history
                               nil nil (thing-at-point 'word)
                               'x/translate-history)))
    (xwidget-webkit-browse-url
     (format x/translate-to-zh-url
             (url-hexify-string text)))))

(defun x/translate-to-en ()
  "Translate TEXT to en."
  (interactive)
  (let ((text (completing-read "2en: "
                               x/translate-history
                               nil nil (thing-at-point 'word)
                               'x/translate-history)))
    (xwidget-webkit-browse-url
     (format x/translate-to-en-url
             (url-hexify-string text)))))

;;; insert-translated-text
(autoload #'x/insert-translated-text "x-insert-translated-text" "Automatic insertion of translated content using DeepL." t)

;; https://emacs-china.org/t/emacs-macos-ocr/23071
(defun x/siri-ocr ()
  "Run ocr and put result to the clipboard."
  (interactive)
  (shell-command "shortcuts run \"OCR Selected Area\"")
  (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))

;; (x/package-use 'websocket)
;; (x/package-use '(deno-bridge . "manateelazycat/deno-bridge"))
;; (x/package-use '(insert-translated-name . ("manateelazycat/insert-translated-name" :files ("*.el" "*.ts"))))
;; (require 'insert-translated-name)

;;; jinx: spell check
(setq jinx-exclude-regexps
      '((emacs-lisp-mode "Package-Requires:.*$")
        (t
         ;; non ASCII
         "\\cc"
         ;; Uppercase words
         "[A-Z]+\\>"
         ;; Words with numbers, hex codes
         "\\w*?[0-9]\\w*\\>"
         ;; URI
         "[a-z]+://\\S-+"
         ;; Email
         "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
         ;; Local variable indicator
         "\\(?:Local Variables\\|End\\):\\s-*$"
         ;; Local variables
         "jinx-\\(?:languages\\|local-words\\):\\s-+.*$")))
(add-hook 'emacs-startup-hook 'global-jinx-mode)

(provide 'x-dictionary)
;;; x-dictionary.el ends here
