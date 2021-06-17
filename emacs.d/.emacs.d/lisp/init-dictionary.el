;;; init-dictionary.el --- init-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(sdcv :host github
        :repo "manateelazycat/sdcv"))

(straight-register-package
 '(english-teacher :host github
                   :repo "loyalpartner/english-teacher.el"))

(leaf sdcv
  :straight t english-teacher go-translate
  :commands (sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-search-input
             sdcv-search-input+)
  :bind
  ("C-c x l" . sdcv-search-pointer+)
  ("C-c x k" . sdcv-search-input+)
  :config
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")
  (setq sdcv-say-word-p nil
        sdcv-tooltip-timeout 10
        sdcv-dictionary-data-dir  (expand-file-name "~/.stardict"))
  :custom
  (english-teacher-backend . 'google)
  (english-teacher-show-result-function . 'english-teacher-eldoc-show-result-function)
  :hook
  ((Info-mode-hook
    Man-mode-hook
    ;; help-mode
    Woman-mode-hook)
   . english-teacher-follow-mode)
  :config
  (setq go-translate-base-url "https://translate.google.cn")
  (setq go-translate-local-language "zh-CN")
  (setq go-translate-token-current (cons 430675 2721866130)))

(provide 'init-dictionary)
;;; init-dictionary.el ends here
