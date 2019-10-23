;;; xandeer/basic/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))

(after! projectile
  (dolist (dir '("~/Downloads/" "~/projects/personal/dotfiles/" "~/.emacs.d/"))
    (when (file-directory-p dir)
      (add-to-list 'projectile-known-projects dir))))

(use-package! youdao-dictionary
  :config
  (setq-default
   url-automatic-caching t
   youdao-dictionary-search-history-file (concat doom-cache-dir "youdao.cache")))

(use-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
