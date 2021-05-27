;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf origami
  :straight t
  :require t
  :bind
  (:origami-mode-map
   ("C-c x z". origami-toggle-node))
  :config
  (global-origami-mode))

(provide 'init-folding)
;;; init-folding.el ends here
