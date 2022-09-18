;;; x-repeat.el --- repeat mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/append-init-hook #'repeat-mode)

(defun repeatize (keymap)
  "Add `repeat-mode' support to KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

;;; scroll
(defvar x/scroll-repeat-map (make-sparse-keymap))
(x/define-keys x/scroll-repeat-map
               '(("j" scroll-up-command)
                 ("k" scroll-down-command)))
;; why it does't work?
(repeatize 'x/scroll-repeat-map)

;;; recenter
(defun x/recenter-top ()
  "Recenter to top."
  (interactive)
  (recenter-top-bottom 1))

(defun x/recenter-bottom ()
  "Recenter to bottom."
  (interactive)
  (recenter-top-bottom -1))

(defvar x/recenter-repeat-map (make-sparse-keymap))
(x/define-keys x/recenter-repeat-map
     '(("l" recenter-top-bottom)
       ("j" x/recenter-top)
       ("k" x/recenter-bottom)))
(repeatize 'x/recenter-repeat-map)

;;; walk buffers
(defvar x/walk-buffers-repeat-map (make-sparse-keymap))
(x/define-keys x/walk-buffers-repeat-map
               '(("j" next-buffer)
                 ("k" previous-buffer)))
(repeatize 'x/walk-buffers-repeat-map)

;;; xref
(with-eval-after-load 'xref
  (defvar x/xref-repeat-map (make-sparse-keymap)
    "Keymap for `x/xref-repeat'.")
(x/define-keys x/xref-repeat-map
               '(("." xref-go-forward)
                 ("," xref-go-back)))
(repeatize 'x/xref-repeat-map))

(provide 'x-repeat)
;;; x-repeat.el ends here
