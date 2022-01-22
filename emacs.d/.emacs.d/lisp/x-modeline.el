;;; x-modeline.el --- x-modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(unless
    (fboundp 'doom-modeline-mode)
  (autoload #'doom-modeline-mode "doom-modeline" nil t))
(unless
    (fboundp 'column-number-mode)
  (autoload #'column-number-mode "doom-modeline" nil t))

(require-package 'doom-modeline)
(require 'x-init-utils)

(x/append-init-hook 'doom-modeline-mode)
(add-hook 'doom-modeline-mode-hook #'column-number-mode)

;; Prevent flash of unstyled modeline at startup
(unless after-init-time
  (setq-default mode-line-format nil))

(setq doom-modeline-buffer-encoding 'nondefault
      doom-modeline-vcs-max-length 12
      doom-modeline-buffer-file-name-style 'file-name
      auto-revert-check-vc-info t)

(setq doom-modeline-buffer-state-icon nil)
(setq doom-modeline-buffer-modification-icon nil)
;; doom-modeline-indent-alist
(setq doom-modeline-indent-info t)
(setq doom-modeline-workspace-name nil)

(provide 'x-modeline)
;;; x-modeline.el ends here
