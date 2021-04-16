;;; xandeer-tools-rime.el --- Xandeer's Emacs Configuration tools rime file.  -*- lexical-binding: t; -*-

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
  :require t
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
  :when (or *is-a-mac* *is-a-linux*)
  :custom
  (rime-librime-root . "~/.local/share/librime")
  (rime-user-data-dir . "~/.cache/rime")
  (rime-show-candidate . 'posframe)
  (rime-show-preedit . t)
  (rime-cursor . "˰")
  (rime-inline-ascii-trigger . 'shift-l)
  (rime-translate-keybindings . '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-`"))
  (rime-disable-predicates .
                           '(rime-predicate-evil-mode-p
                             rime-predicate-after-alphabet-char-p
                             rime-predicate-current-uppercase-letter-p
                             rime-predicate-prog-in-code-p))
  (default-input-method . "rime"))

(provide 'xandeer-tools-rime)
;;; xandeer-tools-rime.el ends here
