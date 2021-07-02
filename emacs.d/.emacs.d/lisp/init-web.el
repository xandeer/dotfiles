;;; init-web.el --- init-web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf web
  :straight web-mode counsel-css company-web json-mode
  :hook (css-mode-hook . counsel-css-imenu-setup)
  :mode (("\\.js\\'" "\\.html\\'" "\\.vue\\'") . web-mode))

(leaf css-mode
  :custom
  (css-indent-offset . 2))

(leaf js
  :mode
  ("\\.js\\'" . js-mode)
  :hook (js-mode-hook . lsp)
  :custom
  (js-indent-level . 2)
  :config
  ;; fix `js-find-symbol' [M-.] overriding other packages' keybinding.
  (substitute-key-definition 'js-find-symbol 'xref-find-definitions js-mode-map))

(leaf ob-js
  :after org
  :commands org-babel-execute:js
  :init
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))
  (add-to-list 'org-babel-default-header-args:js '(:results . "output")))

(leaf ob-deno
  :disabled t
  :commands org-babel-execute:deno
  :config
  (add-to-list 'org-babel-load-languages '(deno . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("deno" . "js"))
  (add-to-list 'org-babel-default-header-args:deno '(:results . "output"))
  ;; optional (require the typescript.el)
  ;; (add-to-list 'org-src-lang-modes '("deno" . typescript))
  )

(leaf npm-mode
  :straight t
  :hook (js-mode-hook . npm-mode))

(leaf npm
  :straight t
  :commands npm-menu)

(leaf typescript-mode
  :straight t
  :hook (typescript-mode-hook . lsp)
  :custom
  (typescript-indent-level . 2))

(straight-register-package
 '(ob-typescript :host github
                 :repo "lurdan/ob-typescript"))
(leaf ob-typescript
  :straight t
  :require t
  :after org
  :commands org-babel-execute:typescript
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-default-header-args:typescript '(:results . "output")))

(leaf tide
  :straight t
  :after typescript-mode
  :commands tide-mode
  :hook
  (typescript-mode-hook . tide-setup)
  (typescript-mode-hook . tide-hl-identifier-mode)
  (before-save-hook . tide-format-before-save)
  :bind
  ("C-x f". tide-format)
  (:tide-mode-map
   :package tide
   ("M-." . lsp-ui-peek-find-definitions)
   ("M-," . lsp-ui-peek-find-references)))

(provide 'init-web)
;;; init-web.el ends here
