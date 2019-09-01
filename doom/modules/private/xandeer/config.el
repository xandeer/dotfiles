;;; private/xandeer/config.el -*- lexical-binding: t; -*-

;;
;;; Theme
;; (load-theme 'doom-dracula t)
(load-theme 'nord t)

(setq
 ;doom-font (font-spec :family "Fira Mono for Powerline" :size 15)
 doom-font (font-spec :family "Consola Mono" :size 16)
 scroll-margin 0
 display-line-numbers-type 'relative)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
;;
;;; Key bindings
(map! :leader
  :desc "Switch buffer"  "."    #'switch-to-buffer
  :desc "Select the treemacs window if it is visible"    "z"    #'treemacs-select-window
  :desc "Agenda List"    "a"    #'org-agenda-list)

(map!
 :i "C-d" #'delete-char
 :i "C-f" #'forward-char
 :i "C-b" #'backward-char)

(map! :map override
      ;; override for org mode
      :i "C-d" #'delete-char

      :gni "M-h" #'+workspace/switch-left
      :gni "M-l" #'+workspace/switch-right

      :i "C-y" #'yank
      :i "M-y" #'yank-pop
      :i "C-r" #'isearch-backward
      )

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
  (dolist (dir '("~/Downloads/" "~/.config/doom/" "~/Dropbox/notes/" "~/.emacs.d/"))
    (when (file-directory-p dir)
      (add-to-list 'projectile-known-projects dir))))
;;
;;; Bootstrap configs
(load! "+utils")
(load! "+org")
(load! "+agenda")
(load! "+capture")
(load! "+evil")
(load! "+eshell")
