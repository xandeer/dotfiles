;;; init-mouse.el --- init-mouse -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf disable-mouse
  :straight t
  :init
  (setq disable-mouse-wheel-events nil)
  :config
  (global-disable-mouse-mode))

(provide 'init-mouse)
;;; init-mouse.el ends here
