;;; x-web.el --- x-web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'web-mode)
(require-package 'company-web)
(add-to-list 'auto-mode-alist
             '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.vue\\'" . web-mode))

;; (require-package 'counsel-css)
;; (add-hook 'css-mode-hook #'counsel-css-imenu-setup)

(add-to-list 'auto-mode-alist
             '("\\.js\\'" . js-mode))
(add-hook 'js-mode-hook #'lsp)

(with-eval-after-load 'js-mode
  ;; fix `js-find-symbol' [M-.] overriding other packages' keybinding.
  (substitute-key-definition 'js-find-symbol 'xref-find-definitions js-mode-map))

(unless
    (fboundp 'org-babel-execute:js)
  (autoload #'org-babel-execute:js "ob-js" nil t))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
               '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts
               '("js" . "js"))
  (add-to-list 'org-babel-default-header-args:js
               '(:results . "output")))

(require-package
 '(yarn :host github
        :repo "jmfirth/yarn.el"))

(require-package 'typescript-mode)
;; (add-hook 'typescript-mode-hook #'lsp)

(require-package 'tide)
(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook #'tide-hl-identifier-mode)
  ;; (add-hook 'before-save-hook #'tide-format-before-save)
  )
(with-eval-after-load 'tide
  (define-key tide-mode-map (kbd "C-x f") #'tide-format)
  (define-key tide-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
  (define-key tide-mode-map (kbd "M-,") #'lsp-ui-peek-find-references))

(require-package 'skewer-mode)
(setq skewer-bower-cache-dir (no-littering-expand-var-file-name "skewer-cache"))

(provide 'x-web)
;;; x-web.el ends here
