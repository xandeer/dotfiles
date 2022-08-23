;;; x-book.el --- book -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; nov
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(defun x/nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Bookerly"
                           :height 1.0))
(add-hook 'nov-mode-hook #'x/nov-font-setup)
(with-eval-after-load 'nov
  (x/define-keys nov-mode-map '(("f" . x/link-hint-open-in-current-window))))

(provide 'x-book)
;;; x-book.el ends here
