;;; x-devdocs.el --- x-devdocs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'devdocs)

(defun x/setup-devdocs (docs)
  (lambda () (setq-local devdocs-current-docs docs)))

(add-hook 'css-mode-hook (x/setup-devdocs '("css")))
(add-hook 'elixir-mode-hook (x/setup-devdocs '("elixir~1.13")))
(add-hook 'kotlin-mode-hook (x/setup-devdocs '("kotlin~1.6" "openjdk~1.8")))
(add-hook 'typescript-mode-hook (x/setup-devdocs '("typescript")))

(provide 'x-devdocs)
;;; x-devdocs.el ends here
