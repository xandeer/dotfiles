;;; init-kt.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf kt
  :straight kotlin-mode flycheck-kotlin
  :hook
  (kotlin-mode-hook . flycheck-kotlin-setup)
  :init
  (add-to-list 'exec-path (expand-file-name
        "server/build/install/server/bin"
        "~/projects/others/kotlin-language-server"))
  :custom
  (kotlin-tab-width . 2))

(provide 'init-kt)
;;; init-kt.el ends here
