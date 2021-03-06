;;; init-meow.el --- Meow keybindings -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; There's a bug when reload it.
(delete-directory (expand-file-name "straight/build/meow" user-emacs-directory) t)

(defun xr/meow-setqs ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty))

(defun xr/meow-define-motion-keys ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)))

(defun xr/meow-define-leader-keys ()
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; high frequency keybindings
   '("e" . "C-x C-e")
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("," . "M-,")
   ;; window management
   '("w" . other-window)
   '("W" . delete-window)
   '("o" . delete-other-windows)
   '("s" . xandeer/split-window)
   ;; high frequency commands
   '(";" . comment-dwim)
   '("k" . kill-this-buffer)
   '("p" . project-find-file)
   '("d" . dired)
   '("b" . switch-to-buffer)
   '("f" . org-roam-find-file)
   '("i" . imenu)
   '("a" . magit)
   ;; toggles
   '("L" . display-line-numbers-mode)
   '("t" . telega)
   '("r" . org-roam-mode)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)))

(defun xr/meow-define-normal-keys ()
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '("," . meow-beginning-of-thing)
   '("." . meow-end-of-thing)
   '("[" . meow-last-buffer)
   '("a" . meow-append)
   '("A" . meow-append-at-end)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("C" . meow-change-save)
   '("d" . meow-page-down)
   '("D" . meow-page-up)
   '("e" . meow-next-word)
   '("E" . scroll-up-line)
   '("f" . meow-find-expand)
   '("F" . meow-find-expand)
   '("g" . meow-begin-of-buffer)
   '("G" . meow-end-of-buffer)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-insert-at-begin)
   '("j" . meow-next)
   '("J" . avy-goto-line)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-line)
   '("m" . meow-join)
   '("M" . meow-extend)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-block)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till-expand)
   '("T" . meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-page-down)
   '("V" . meow-page-up)
   '("w" . xr/ace-pinyin-goto-word-1)
   '("W" . meow-mark-symbol)
   '("x" . delete-char)
   '("X" . meow-kmacro-lines)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . meow-query-replace-regexp)
   '("'" . repeat)
   '("\\" . quoted-insert)
   '("\/" . meow-visit)
   ;;     '("<escape>" . meow-last-buffer)
   '("<escape>" . meow-cancel)
   '("<f3>" . meow-start-kmacro)
   '("<f4>" . meow-end-or-call-kmacro)))

(defun xr/meow-enable-local-insert ()
  "Set insert as default mode."
  (setq-default meow-normal-mode nil)
  (setq-default meow-insert-mode t))

(defun xr/meow-setup ()
  "Meow setup."
  (xr/meow-setqs)
  (xr/meow-define-motion-keys)
  (xr/meow-define-leader-keys)
  (xr/meow-define-normal-keys))

(advice-add 'meow--global-enable :after 'xr/meow-enable-local-insert)

(leaf meow
  :straight t
  :require t
  :init
  (meow-global-mode 1)
  :bind
  ("C-x C-r" . meow-block)
  :config
  (xr/meow-setup))

(provide 'init-meow)
;;; init-meow.el ends here
