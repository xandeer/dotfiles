;;; xandeer/key-bindings/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun evil-custom-end-of-buffer-dwim (&rest _)
  "If current line is epty, call `previous-line'."
  (when (looking-at-p "^$")
    (previous-line)))

;;;###autoload
(advice-add #'end-of-buffer :after #'evil-custom-end-of-buffer-dwim)
