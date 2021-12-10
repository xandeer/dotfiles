;;; x-dictionary.el --- x-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(sdcv :host github
        :repo "manateelazycat/sdcv"))

(straight-register-package
 '(english-teacher :host github
                   :repo "loyalpartner/english-teacher.el"))

(require-package 'posframe)
(require-package 'sdcv)

(autoload #'sdcv-search-pointer "sdcv" nil t)
(autoload #'sdcv-search-input "sdcv" nil t)
(with-eval-after-load 'sdcv
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")
  (setq sdcv-say-word-p nil)
  (setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict"))
  (global-set-key (kbd "C-c x l") 'sdcv-search-pointer)
  (global-set-key (kbd "C-c x k") 'sdcv-search-input))

(require-package 'go-translate)
;; (require 'go-translate)

(autoload #'go-translate "go-translate" nil t)
(with-eval-after-load 'go-translate
  (setq go-translate-token-current (cons 430675 2721866130))
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-target-language "en")
  (setq go-translate-buffer-follow-p t) ; 翻译完成后，总是将光标切换到翻译结果窗口
  (setq go-translate-buffer-source-fold-p t) ; 在结果页面，折叠源文本。可以通过回车或鼠标点击展开
  (setq go-translate-buffer-line-wrap-p nil) ; 在结果页面，不允许过长的行折行显示
  (setq go-translate-extra-directions '(("zh-CN" . "zh-TW"))))

(provide 'x-dictionary)
;;; x-dictionary.el ends here
