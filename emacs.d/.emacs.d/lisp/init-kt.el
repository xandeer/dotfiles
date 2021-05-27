;;; init-kt.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf kotlin
  :straight kotlin-mode flycheck-kotlin
  :hook
  (kotlin-mode-hook . flycheck-kotlin-setup))

(provide 'init-kt)
;;; init-kt.el ends here
