;;; x-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(mapc (fn (add-hook % #'hs-minor-mode))
      '(emacs-lisp-mode-hook
        kotlin-mode-hook
        cider-mode-hook
        typescript-mode-hook))

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist '(kotlin-mode "{" "}" "/[*/]" nil nil))
  (define-key hs-minor-mode-map (kbd "M-h") #'x/meta-h-map))

(unless (boundp 'x/meta-h-map)
  (define-prefix-command 'x/meta-h-map))

(x/define-keys
 x/meta-h-map
 '(("h" hs-show-block)
   ("M-h" hs-hide-block)
   ("j" hs-hide-all)
   ("M-j" hs-show-all)
   ;; doesn't work as expected
   ;; ("k"    hs-toggle-hiding)
   ("l" hs-hide-level)))

;;; ts-fold
;; (x/package-use '(ts-fold "emacs-tree-sitter/ts-fold"))
;; (require 'ts-fold)

(provide 'x-folding)
;;; x-folding.el ends here
