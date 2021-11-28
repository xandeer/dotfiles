;;; x-jieba.el --- x-jieba -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(jieba
   :host github
   :repo "cireu/jieba.el"
   :files ("*.el" "*.json" "*.js")))

;; 使用 jieba 中文分词
;; prepare: cd ~/.emacs.d/straight/repos/jieba.el; yarn
(leaf jieba
  :straight t
  :commands jieba-mode
  :hook
  (after-init-hook . jieba-mode))

(provide 'x-jieba)
;;; x-jieba.el ends here
