;;; init-keybindings.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf leaf
  :custom
  (mac-option-modifier        . 'meta)
  (mac-command-modifier       . 'hyper)
  (mac-right-command-modifier . 'super)
  (mac-function-modifier      . 'super)
  :bind
  ([remap newline] . newline-and-indent)
   ;; cursor Movement
  ("H-<up>"   . beginning-of-buffer)
  ("H-<down>" . end-of-buffer)
  ("H-l"      . goto-line)

   ;; text Operations
  ("H-a" . mark-whole-buffer)
  ("H-v" . yank)
  ("H-c" . kill-ring-save)
  ("H-s" . save-buffer)
  ("H-z" . undo)
  ("H-n" . make-frame))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
