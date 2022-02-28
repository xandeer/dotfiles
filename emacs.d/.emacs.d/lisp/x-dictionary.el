;;; x-dictionary.el --- x-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package
 '(sdcv :host github
        :repo "manateelazycat/sdcv"))

;; (require-package
;;  '(english-teacher :host github
;;                    :repo "loyalpartner/english-teacher.el"))

(require-package 'posframe)
(require-package 'sdcv)

(autoload #'sdcv-search-pointer "sdcv" nil t)
(autoload #'sdcv-search-input "sdcv" nil t)

(setq sdcv-program (executable-find "sdcv"))
(setq sdcv-say-word-p nil)
(setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict"))

(with-eval-after-load 'sdcv
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")

  (defun x--sdcv-translate-br (s)
    (s-replace "<br>" "\n" s))
  (advice-add 'sdcv-filter :filter-return #'x--sdcv-translate-br))

(require-package 'go-translate)
;; (require 'go-translate)

(autoload #'facemenu-add-face "facemenu" nil t)
(autoload #'gts-do-translate "go-translate" nil t)

(setq gts-translate-list '(("zh" "en")))
(setq gts-translate-token-current (cons 430675 2721866130))
;; (setq gts-translate-base-url "https://translate.google.cn")
;; (setq gts-translate-local-language "zh-CN")
;; (setq gts-translate-target-language "en")
;; (setq gts-translate-buffer-follow-p t)
                                        ; 翻译完成后, 总是将光标切换到翻译结果窗口
;; (setq gts-translate-buffer-source-fold-p t)
                                        ; 在结果页面, 折叠源文本. 可以通过回车或鼠标点击展开
;; (setq gts-translate-buffer-line-wrap-p nil)
                                        ; 在结果页面, 不允许过长的行折行显示
;; (setq gts-translate-extra-directions '(("zh-CN" . "zh-TW")))

(provide 'x-dictionary)
;;; x-dictionary.el ends here
