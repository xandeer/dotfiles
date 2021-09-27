;;; init-dictionary.el --- init-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(sdcv :host github
        :repo "manateelazycat/sdcv"))

(straight-register-package
 '(english-teacher :host github
                   :repo "loyalpartner/english-teacher.el"))

(leaf osx-dictionary
  :disabled t
  :straight t
  :bind
  ("C-c x l" . osx-dictionary-search-word-at-point)
  ("C-c x k" . osx-dictionary-search-input))

(leaf fanyi
  :disabled t
  :straight t
  :require t
  :custom
  (fanyi-providers . '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     fanyi-etymon-provider
                     ;; Longman
                     fanyi-longman-provider)))

(leaf sdcv
  :straight t
  :commands (sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-search-input
             sdcv-search-input+)
  :bind
  ("C-c x l" . sdcv-search-pointer)
  ("C-c x k" . sdcv-search-input)
  :config
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")
  :custom
  (sdcv-say-word-p . nil)
  `(sdcv-dictionary-data-dir . ,(expand-file-name "~/.stardict")))

(leaf english-teacher
  :straight t
  :commands english-teacher-follow-mode
  :custom
  (english-teacher-backend . 'google)
  (english-teacher-show-result-function . 'english-teacher-eldoc-show-result-function)
  :hook
  ((Info-mode-hook
    Man-mode-hook
    ;; help-mode
    Woman-mode-hook)
   . english-teacher-follow-mode))

(leaf go-translate
  :straight t
  :bind
  ("C-c x g" . go-translate)
  :config
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-target-language "en")
  (setq go-translate-buffer-follow-p t)       ; 翻译完成后，总是将光标切换到翻译结果窗口
  (setq go-translate-buffer-source-fold-p t)  ; 在结果页面，折叠源文本。可以通过回车或鼠标点击展开
  (setq go-translate-buffer-line-wrap-p nil)  ; 在结果页面，不允许过长的行折行显示
  (setq go-translate-extra-directions '(("zh-CN" . "zh-TW"))))

(provide 'init-dictionary)
;;; init-dictionary.el ends here
