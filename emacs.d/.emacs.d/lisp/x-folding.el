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
          typescript-mode-hook))

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist '(kotlin-mode "{" "}" "/[*/]" nil nil))
  (define-key hs-minor-mode-map (kbd "M-h") #'x/meta-h-map))

(unless (boundp 'x/meta-h-map)
  (define-prefix-command 'x/meta-h-map))

(x/define-keys
 x/meta-h-map
 '(("h"    hs-show-block)
   ("M-h"  hs-hide-block)
   ("j"    hs-hide-all)
   ("M-j"  hs-show-all)
   ("k"    hs-toggle-hiding) ; doesn't work as expected
   ("l"    hs-hide-level)))

;; (global-set-key (kbd "M-h") #'x/folding-map)

(provide 'x-folding)
;;; x-folding.el ends here
