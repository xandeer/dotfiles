;;; x-javascript.el --- javascript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.c?js\\'" . js2-mode))

(setq js-chain-indent t)
(setq js2-basic-offset 2)
(setq js2-strict-missing-semi-warning nil)

;; (add-hook 'js2-mode-hook #'lsp)

(add-hook 'js2-mode-hook #'skewer-mode)
(add-hook 'css-mode-hook #'skewer-css-mode)
(add-hook 'html-mode-hook #'skewer-html-mode)

;;; repl
(setq skewer-bower-cache-dir (no-littering-expand-var-file-name "skewer-cache"))
(with-eval-after-load 'js2-mode
  ;; fix `js-find-symbol' [M-.] overriding other packages' keybinding.
  (substitute-key-definition 'js-find-symbol 'xref-find-definitions js-mode-map)

  (x/major-mode-lighter 'js2-mode "JS"))

;;; org babel
(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

  (unless (fboundp 'org-babel-execute:js)
    (autoload #'org-babel-execute:js "ob-js" nil t)))

(provide 'x-javascript)
;;; x-javascript.el ends here
