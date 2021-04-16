;;; xandeer-langs.el --- Xandeer's emacs.d init langs file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's emacs.d init langs file.

;;; Code:

(straight-use-package 'kotlin-mode)
(straight-use-package 'flycheck-kotlin)
(leaf flycheck-kotlin
  :defer-config
  (xandeer/add-company-backend 'kotlin-mode 'company-tabnine)
  :hook
  (kotlin-mode-hook . flycheck-kotlin-setup))

(straight-use-package 'geiser)
(leaf geiser
  :config
  (setq geiser-active-implementations '(guile)))

(straight-use-package 'typescript-mode)
(straight-use-package 'tide)
(leaf tide
  :after typescript-mode company flycheck
  :require t
  :bind
  ("C-x f". tide-format)
  :defer-config
  (xandeer/add-company-backend 'typescript-mode 'company-tabnine)
  :hook
  (typescript-mode-hook . tide-setup)
  (typescript-mode-hook . tide-hl-identifier-mode)
  (before-save . tide-format-before-save))

(provide 'xandeer-langs)
;;; xandeer-langs.el ends here
