;;; x-rime.el --- Xandeer's Emacs Configuration tools rime file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor Rime.

;;; Code:

(with-eval-after-load 'rime
  (setq rime-user-data-dir "~/.cache/rime")
  (setq rime-librime-root "~/projects/personal/dotfiles/librime")
  (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@29/include/")
  (setq rime-show-candidate 'posframe)
  (setq rime-show-preedit t)
  (setq rime-cursor "˰")
  (setq rime-inline-ascii-trigger 'shift-l)
  (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "<left>" "<right>" "<up>" "<down>" "<prior>" "<next>" "<delete>" "C-`"))

  (defun x/rime--telega-msg-p ()
    (and (fboundp 'telega-msg-at) (telega-msg-at)))

  (setq rime-disable-predicates
        '(rime-predicate-after-alphabet-char-p
          rime-predicate-current-uppercase-letter-p
          meow-normal-mode-p
          x/ace-mode-p
          x/tbl-mode-p
          ;; x-point-speed-activate-p
          x/org-heading-beginning-p
          rime-predicate-hydra-p
          rime-predicate-ace-window-p
          rime-predicate-prog-in-code-p
          rime-predicate-space-after-cc-p
          x/rime--telega-msg-p))

  (define-key rime-mode-map (kbd "M-I") 'rime-force-enable)
  (define-key rime-active-mode-map (kbd "M-i") 'rime-inline-ascii))

(global-set-key (kbd "M-i") 'toggle-input-method)

(setq default-input-method "rime")

(defcustom x/rime-ignored-adim-buffers
  '("COMMIT_EDITMSG"
    "CAPTURE-inbox.org"
    "inbox.org")
  "When `buffer-name is contained in it, don't active default input method.")

(defun x/rime-ignored-adim-buffer-p ()
  "Predicate for checking `x/rime-ignored-adim-buffers."
  (member (buffer-name) x/rime-ignored-adim-buffers))

(defvar x/rime-ignored-adim-preds
  '(x/rime-ignored-adim-buffer-p
    x/tbl-mode-p)
  "Predicates for ignoring to active default input method.")

(defun x/activate-default-input-method ()
  (unless (-any? (lambda (p) (funcall p)) x/rime-ignored-adim-preds)
    (activate-input-method default-input-method)))

(add-hook 'text-mode-hook #'x/activate-default-input-method)
(add-hook 'telega-chat-mode-hook #'x/activate-default-input-method)

(defun x/highlight-cursor ()
  (set-face-background
   'cursor
   (if (and (bound-and-true-p rime-mode)
            (rime--should-enable-p)
            (not (rime--should-inline-ascii-p)))
       "#f48225"
     "#51afef")))

(defvar x/last-post-command-position 0
  "Holds the cursor position from the last run of post-command-hooks.")

(make-variable-buffer-local 'x/last-post-command-position)
(defvar x/cursor-move-hook nil
  "Holds the cursor move hook from the last run of `post-command-hook'.")

(defun x/do-stuff-if-moved-post-command ()
  (unless (equal (point) x/last-post-command-position)
    (run-hooks 'x/cursor-move-hook))
  (setq x/last-post-command-position (point)))

;; (add-hook 'post-command-hook #'x/do-stuff-if-moved-post-command)
;; (remove-hook 'post-command-hook #'x/do-stuff-if-moved-post-command)
(add-hook 'post-command-hook #'x/highlight-cursor)
;; (add-hook 'x/cursor-move-hook #'x/highlight-cursor)
;; (add-function :after after-focus-change-function
;;               #'x/highlight-cursor)

(provide 'x-rime)
;;; x-rime.el ends here
