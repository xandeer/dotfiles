;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'origami)
(leaf origami
  :require t
  :bind
  (:origami-mode-map
   ("C-c x z". origami-toggle-node))
  :config
  (global-origami-mode))


(provide 'init-folding)
;;; init-folding.el ends here
