;;; init-kt.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(straight-use-package 'kotlin-mode)
(straight-use-package 'flycheck-kotlin)
(leaf flycheck-kotlin
  :hook
  (kotlin-mode-hook . flycheck-kotlin-setup))

(provide 'init-kt)
;;; init-kt.el ends here
