;;; init-kt.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf kotlin-mode
  :straight flycheck-kotlin t
  :hook
  (kotlin-mode-hook . flycheck-kotlin-setup)
  :init
  (add-to-list 'exec-path (expand-file-name
        "server/build/install/server/bin"
        "~/projects/others/kotlin-language-server"))
  :custom
  (kotlin-tab-width . 2))

(straight-register-package
 '(ob-kotlin :host github
             :repo "zweifisch/ob-kotlin"))
(leaf ob-kotlin
  :straight t
  :after org
  :commands org-babel-execute:kotlin
  :init
  (add-to-list 'org-babel-load-languages '(kotlin . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("kotlin" . "kt")))
(provide 'init-kt)
;;; init-kt.el ends here
