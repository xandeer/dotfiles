;;; x-jieba.el --- x-jieba -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 使用 jieba 中文分词
;; prepare: cd ~/.emacs.d/straight/repos/jieba.el; yarn
(x/append-init-hook #'jieba-mode)

(provide 'x-jieba)
;;; x-jieba.el ends here
