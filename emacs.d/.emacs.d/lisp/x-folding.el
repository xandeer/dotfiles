;;; x-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;
;; Copyright (C) 2022 Kevin Du
;;
;; Author: Kevin Du <xandeer@gmail.com>
;; Maintainer: Kevin Du <xandeer@gmail.com>
;; Created: August 04, 2022
;; Modified: August 04, 2022
;; Version: 0.0.1
;; Keywords: fold hide show
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Folding with `hs-minor-mode'.
;;
;;; Code:

(mapcar (lambda (hook) (add-hook hook #'hs-minor-mode))
        '(emacs-lisp-mode-hook
          kotlin-mode-hook
          cider-mode-hook
          tide-mode-hook))

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist '(kotlin-mode "{" "}" "/[*/]" nil nil)))

(define-prefix-command 'x/folding-map)

(x/define-keys
 x/folding-map
 '(("h" . hs-show-block)
   ("M-h" . hs-hide-block)
   ("j" . hs-hide-all)
   ("M-j" . hs-show-all)
   ;; doesn't work as expected
   ("k" . hs-toggle-hiding)
   ("l" . hs-hide-level)))

(global-set-key (kbd "M-h") #'x/folding-map)

(provide 'x-folding)
;;; x-folding.el ends here
