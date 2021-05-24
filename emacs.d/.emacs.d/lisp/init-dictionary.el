;;; init-dictionary.el --- init-dictionary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
'(sdcv :host github
  :repo "manateelazycat/sdcv"))

(straight-use-package 'sdcv)
(leaf sdcv
  :commands (sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-search-input
             sdcv-search-input+)
  :bind
  ("C-c x l" . sdcv-search-pointer+)
  ("C-c x k" . sdcv-search-input+)
  :config
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")
  (setq sdcv-say-word-p nil
        sdcv-tooltip-timeout 10
        sdcv-dictionary-data-dir  (expand-file-name "~/.stardict")))

(provide 'init-dictionary)
;;; init-dictionary.el ends here
