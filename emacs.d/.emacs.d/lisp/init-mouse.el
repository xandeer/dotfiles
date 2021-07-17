;;; init-mouse.el --- init-mouse -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf disable-mouse
  :straight t
  :require t
  :init
  (setq disable-mouse-wheel-events nil)
  :bind
  :config
  (global-disable-mouse-mode))

(leaf mwheel
  :custom
  (mouse-wheel-scroll-amount . '(1))
  (mac-mouse-wheel-smooth-scroll . t))

(provide 'init-mouse)
;;; init-mouse.el ends here
