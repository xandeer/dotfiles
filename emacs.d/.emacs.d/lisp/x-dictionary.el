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
      (azure-tts-play word)))

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
(setq gt-translate-list '(("zh" "en")))
;; (autoload #'facemenu-add-face "facemenu" nil t) ; required by gts v1, and it's deprecated

(defun x/deepl-key ()
  "Get the api token from th authinfo."
  (auth-source-pick-first-password :host "deepl.com" :user "deepl"))

(with-eval-after-load 'go-translate
  (setq gt-default-translator
        (gt-translator
         :taker   (gt-taker :text 'buffer :pick 'paragraph)  ; config the Taker
         ;; :engines (list (gts-google-engine) (gts-google-rpc-engine))
         ;; :engines `(,(gts-google-engine))
         :engines `(,(gt-bing-engine))
         ;; :engines `(,(gts-deepl-engine :auth-key (x/deepl-key) :pro nil))
         :render (gt-buffer-render)))

  (advice-add 'gt-text :filter-return #'x/text-normalize))

;;; immersive-translate
;; (setq immersive-translate-backend 'chatgpt)
;; (add-hook 'elfeed-show-mode-hook #'immersive-translate-setup)
;; (add-hook 'nov-pre-html-render-hook #'immersive-translate-setup)

;; https://emacs-china.org/t/emacs-macos-ocr/23071
;; (defun x/siri-ocr ()
;;   "Run ocr and put result to the clipboard."
;;   (interactive)
;;   (shell-command "shortcuts run \"OCR Selected Area\"")
;;   (do-applescript "tell application id \"org.gnu.Emacs\" to activate"))

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
(add-hook 'emacs-startup-hook #'global-jinx-mode)

(provide 'x-dictionary)
;;; x-dictionary.el ends here
