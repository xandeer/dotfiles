;;; x-jieba.el --- x-jieba -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; 使用 jieba 中文分词
;; prepare: cd ~/.emacs.d/straight/repos/jieba.el; yarn
(setq jieba-server-start-args '("nix" "run" "nixpkgs#nodejs_18" "--" "simple-jieba-server.js"))
(x/append-init-hook #'jieba-mode)

(provide 'x-jieba)
;;; x-jieba.el ends here
