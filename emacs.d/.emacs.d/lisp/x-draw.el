;;; x-draw.el --- draw -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/andorsk/d2-mode
(x/package-use '(d2-mode . "andorsk/d2-mode"))
(require 'd2-mode)
(setq d2-output-format ".png")
(setq d2-flags "-t 5 -l elk")

;; (setq d2-location "/usr/local/bin/d2")

(provide 'x-draw)
;;; x-draw.el ends here
