;;; init-keybindings.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf leaf
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

(provide 'init-keybindings)
;;; init-keybindings.el ends here
