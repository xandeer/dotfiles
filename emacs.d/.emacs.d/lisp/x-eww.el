;;; x-eww.el --- x-eww -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'eww
  (define-key eww-mode-map (kbd "f") #'link-hint-open-link)
  (define-key eww-mode-map (kbd "d") #'scroll-up-command)
  (define-key eww-mode-map (kbd "e") #'scroll-down-command))

(provide 'x-eww)
;;; x-eww.el ends here
