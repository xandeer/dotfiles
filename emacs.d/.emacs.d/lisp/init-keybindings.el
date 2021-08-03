;;; init-keybindings.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(leaf which-key
  :straight t
  :custom
  (which-key-allow-imprecise-window-fit . nil)
  :hook after-init-hook
  :custom
  (mac-option-modifier        . 'meta)
  (mac-command-modifier       . 'hyper)
  (mac-right-command-modifier . 'hyper)
  (mac-function-modifier      . 'super)
  :init
  (defun xr/switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer)))
  :bind*
  ([remap kill-buffer]            . kill-current-buffer)
  ([remap comment-dwim]           . comment-line)
  :bind
  ([remap move-beginning-of-line] . xr/smart-beginning-of-line)
  ([remap newline]                . newline-and-indent)

  ("M-[" . xr/switch-to-last-buffer)
  ("C-z" . ns-next-frame)

  ("H-<up>"   . beginning-of-buffer)
  ("H-<down>" . end-of-buffer)
  ("H-l"      . goto-line)

  ;; text Operations
  ("H-a" . mark-whole-buffer)
  ("H-c" . kill-ring-save)
  ("H-d" . xr/duplicate-line)
  ("H-s" . save-buffer)
  ("H-v" . yank)

  ("H-z" . undo)

  ("H-n" . make-frame))

(leaf keyfreq
  :straight t
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          org-self-insert-command
          disable-mouse--handle
          forward-char
          backward-char
          previous-line
          next-line
          newline-and-indent))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
