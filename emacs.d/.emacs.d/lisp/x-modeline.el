;;; x-modeline.el --- x-modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf doom-modeline
  :straight t
  :hook after-init-hook
  :hook (doom-modeline-mode-hook . column-number-mode)
  :init
  ;; Prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq-default mode-line-format nil))
  (setq doom-modeline-buffer-encoding 'nondefault
        doom-modeline-major-mode-icon nil
        doom-modeline-vcs-max-length 12
        doom-modeline-buffer-file-name-style 'file-name
        auto-revert-check-vc-info t))

(provide 'x-modeline)
;;; x-modeline.el ends here
