;;; x-rime.el --- Xandeer's Emacs Configuration tools rime file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor Rime.

;;; Code:

(straight-register-package
 '(rime :host github
        :repo "DogLooksGood/emacs-rime"
        :files ( "Makefile" "*.el" "lib.c")))

(require-package 'rime)
(require-package 'posframe)

(setq default-input-method "rime")

(with-eval-after-load 'rime
  (setq rime-user-data-dir "~/.cache/rime")
  (setq rime-librime-root "~/.local/share/librime/dist")
  (setq rime-show-candidate 'posframe)
  (setq rime-show-preedit t)
  (setq rime-cursor "˰")
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-`"))

  (custom-set-variables
   '(rime-disable-predicates
     '(rime-predicate-after-alphabet-char-p
       rime-predicate-current-uppercase-letter-p
       meow-normal-mode-p
       x/ace-mode-p
       x/org-heading-beginning-p
       x/selection-mode-p
       rime-predicate-hydra-p
       rime-predicate-ace-window-p
       rime-predicate-prog-in-code-p
       ;; rime-predicate-punctuation-after-space-cc-p
       rime-predicate-space-after-cc-p)))

  (define-key rime-mode-map (kbd "M-I") 'rime-force-enable)
  (define-key rime-active-mode-map (kbd "M-i") 'rime-inline-ascii))
(global-set-key (kbd "M-i") 'toggle-input-method)

(provide 'x-rime)
;;; x-rime.el ends here
