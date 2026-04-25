;;; x-modeline.el --- x-modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(unless
    (fboundp 'doom-modeline-mode)
  (autoload #'doom-modeline-mode "doom-modeline" nil t))
(unless
    (fboundp 'column-number-mode)
  (autoload #'column-number-mode "doom-modeline" nil t))

(custom-set-faces
 '(mode-line ((t (:slant italic))))
 '(mode-line-active ((t (:slant italic))))
 '(mode-line-inactive ((t (:slant italic)))))

(x/append-init-hook #'doom-modeline-mode)

(defun x/modeline-setup ()
  "Setup for the modeline."
  (column-number-mode 1))
(add-hook 'doom-modeline-mode-hook #'x/modeline-setup)

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
(setq doom-modeline-indent-info nil)
(setq doom-modeline-workspace-name nil)
(setq doom-modeline-window-width-limit 80)

(with-eval-after-load 'which-func
  (setq mode-line-misc-info
        (delq (assq 'which-function-mode mode-line-misc-info) mode-line-misc-info)))

(provide 'x-modeline)
;;; x-modeline.el ends here
