;;; x-dictionary.el --- x-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(autoload #'sdcv-search-pointer "sdcv" nil t)
(autoload #'sdcv-search-input "sdcv" nil t)

(setq sdcv-program (executable-find "sdcv"))
(setq sdcv-say-word-p nil)
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict"))

(with-eval-after-load 'sdcv
  ;; face
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")

  (defun x-sdcv--translate-br (s)
    (s-replace "<br>" "\n" s))
  (advice-add 'sdcv-translate-result :filter-return #'x-sdcv--translate-br)

  ;; say
  (defvar x/sdcv-say-word-p t)

  (defun x-sdcv--say-word (&optional word)
    (when (and x/sdcv-say-word-p
               word
               (not (jieba-chinese-word? word)))
      (osx-lib-say word)))

  (advice-add 'sdcv-search-detail :after #'x-sdcv--say-word)

  (define-key sdcv-mode-map (kbd "s")
              (lambda ()
                (interactive)
                (osx-lib-say sdcv-current-translate-object))))

;;; translate
(autoload #'facemenu-add-face "facemenu" nil t)
(autoload #'gts-translator "go-translate" nil t)

(setq gts-translate-list '(("zh" "en")))

(setq gts-default-translator
      (gts-translator
       :picker (gts-prompt-picker)
       ;; :engines (list (gts-google-engine) (gts-google-rpc-engine))
       ;; :engines `(,(gts-google-engine))
       ;; :engines `(,(gts-bing-engine))
       :engines `(,(gts-deepl-engine :auth-key x/deepl-key :pro nil))
       :render (gts-buffer-render)))

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

(provide 'x-dictionary)
;;; x-dictionary.el ends here
