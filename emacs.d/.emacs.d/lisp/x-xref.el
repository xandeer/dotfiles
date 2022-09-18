;;; x-xref.el --- code references -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar x/xref-repeat-map
  (let ((map (make-sparse-keymap)))
    (x/define-keys map
                   '(("." xref-go-forward)
                     ("," xref-go-back)))
    map)
  "Keymap for `x/xref-repeat'.")

(put 'xref-go-back 'repeat-map 'x/xref-repeat-map)
(put 'xref-go-forward 'repeat-map 'x/xref-repeat-map)

(provide 'x-xref)
;;; x-xref.el ends here
