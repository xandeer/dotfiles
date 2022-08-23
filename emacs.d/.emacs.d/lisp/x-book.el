;;; x-book.el --- book -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; info
(defconst x/info-like-map '(("b" . Info-history-back)
                            ("d" . Info-scroll-up)
                            ("e" . Info-scroll-down)
                            ("f" . x/link-hint-open-in-current-window)
                            ("l" . sdcv-search-pointer)))

(with-eval-after-load 'info
  (x/define-keys Info-mode-map x/info-like-map))

;;; nov
(with-eval-after-load 'nov
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (defun x/nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Bookerly"
                             :height 1.0))
  (add-hook 'nov-mode-hook #'x/nov-font-setup)
  (x/define-keys nov-mode-map x/info-like-map))

(provide 'x-book)
;;; x-book.el ends here
