;;; init-rime.el --- Xandeer's Emacs Configuration tools rime file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor Rime.

;;; Code:

(straight-register-package
 '(rime :host github
        :repo "DogLooksGood/emacs-rime"
        :files ( "Makefile" "*.el" "lib.c")))

(straight-use-package 'rime)
(straight-use-package 'posframe)

(leaf rime
  :bind (("M-i" . toggle-input-method))
  :bind (:rime-mode-map
         ("M-I" . rime-force-enable))
  :bind (:rime-active-mode-map
         ("M-i" . rime-inline-ascii))
  :bind (:ivy-minibuffer-map
         ("M-i" . toggle-input-method))
  :config
  (when *is-a-linux*
    (setq rime-emacs-module-header-root "/nix/store/m0bvchfp8b8ddnpjqn36n82f1wgl3qza-emacs-27.1/include")
    (setq rime-share-data-dir (expand-file-name "~/.config/fcitx/rime")))
  (setq rime-librime-root "~/.local/share/librime")
  (setq rime-user-data-dir "~/.cache/rime")
  (setq rime-show-candidate 'posframe)
  (setq rime-show-preedit t)
  (setq rime-cursor "˰")
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-`"))
  (custom-set-variables
   '(rime-disable-predicates
     '(rime-predicate-evil-mode-p
       rime-predicate-after-alphabet-char-p
       rime-predicate-current-uppercase-letter-p
       meow-normal-mode-p
       xr/ace-mode-p
       rime-predicate-hydra-p
       rime-predicate-ace-window-p
       rime-predicate-prog-in-code-p)))
  (setq default-input-method "rime"))

(provide 'init-rime)
;;; init-rime.el ends here
