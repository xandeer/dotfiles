;;; init-web.el --- init-web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf web
  :straight web-mode counsel-css company-web json-mode
  :hook (css-mode-hook . counsel-css-imenu-setup)
  :mode (("\\.js\\'" "\\.html\\'" "\\.vue\\'"). web-mode))

(provide 'init-web)
;;; init-web.el ends here
