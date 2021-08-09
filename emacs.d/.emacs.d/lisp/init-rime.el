;;; init-rime.el --- Xandeer's Emacs Configuration tools rime file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor Rime.

;;; Code:

(straight-register-package
 '(rime :host github
        :repo "DogLooksGood/emacs-rime"
        :files ( "Makefile" "*.el" "lib.c")))

(leaf rime
  :straight t posframe
  :bind ("M-i" . toggle-input-method)
  :bind (:rime-mode-map
         ("M-I" . rime-force-enable))
  :bind (:rime-active-mode-map
         ("M-i" . rime-inline-ascii))
  :bind (:ivy-minibuffer-map
         ("M-i" . toggle-input-method))
  :custom
  (default-input-method . "rime")
  (rime-librime-root    . "~/.local/share/librime/dist")
  ;; (rime-share-data-dir  . "~/.local/share/librime/thirdparty/share") ; rime-deploy doesn't work
  (rime-share-data-dir  . "/Library/Input Methods/Squirrel.app/Contents/SharedSupport")
  (rime-user-data-dir   . "~/.cache/rime")
  :config
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
       xr/org-heading-beginning-p
       rime-predicate-hydra-p
       rime-predicate-ace-window-p
       rime-predicate-prog-in-code-p))))

(provide 'init-rime)
;;; init-rime.el ends here
