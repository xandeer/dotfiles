;;; x-wakatime.el --- x-wakatime -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'wakatime-mode)
(setq wakatime-cli-path (expand-file-name "/opt/homebrew/bin/wakatime-cli"))
(global-wakatime-mode)

(provide 'x-wakatime)
;;; x-wakatime.el ends here
