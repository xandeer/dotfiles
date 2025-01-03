;;; x-draw.el --- draw -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/andorsk/d2-mode
(x/package-use '(d2-mode . "andorsk/d2-mode"))
(require 'd2-mode)
(setq d2-output-format ".png")

(defvar x/d2-layout "tala"
  "D2 layout engine.
There're three engines available:
  - dagre(default)
  - elk
  - tala")
;; (setq d2-flags "-t 5 -l elk")
(setq d2-flags (concat  "-t 5 -l " x/d2-layout))

(setq d2-location (expand-file-name "d2" "~/bin"))

(provide 'x-draw)
;;; x-draw.el ends here
