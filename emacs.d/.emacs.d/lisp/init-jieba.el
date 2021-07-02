;;; init-jieba.el --- init-jieba -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(jieba
   :host github
   :repo "cireu/jieba.el"
   :files ("*.el" "*.json" "*.js")))

;; 使用 jieba 中文分词
(leaf jieba
  :straight t
  :commands jieba-mode
  :init
  (jieba-mode))

(provide 'init-jieba)
;;; init-jieba.el ends here
