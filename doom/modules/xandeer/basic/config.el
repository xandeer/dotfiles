;;; xandeer/basic/config.el -*- lexical-binding: t; -*-

(after! ivy
  (map! :map ivy-minibuffer-map
        "C-d" (λ! (insert (format-time-string "Daily %Y-%m-%d" (current-time))))
        "C-w" (λ! (insert (format-time-string "Words %Y-%m-%d" (current-time))))
        "C-v" #'ivy-scroll-up-command))

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc" . sh-mode))

(after! projectile
  (dolist (dir '("~/Downloads/" "~/projects/personal/dotfiles" "~/Dropbox/notes/" "~/.emacs.d/"))
    (when (file-directory-p dir)
      (add-to-list 'projectile-known-projects dir))))

(use-package! youdao-dictionary
  :config
  (setq-default
   url-automatic-caching t
   youdao-dictionary-search-history-file (concat doom-cache-dir "youdao.cache")))
