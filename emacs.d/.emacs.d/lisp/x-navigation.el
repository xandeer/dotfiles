;;; x-navigation.el --- Basic navigation map -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Kevin Du
;;
;; Author: Kevin Du <xandeer@gmail.com>
;; Maintainer: Kevin Du <xandeer@gmail.com>
;; Created: July 13, 2022
;; Modified: July 13, 2022
;; Version: 0.0.1
;; Keywords: navigaion
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Basic navigation map
;;
;;; Code:

(define-prefix-command 'x/navigation-map)

(x/define-keys
 x/navigation-map
 '(("a" . beginning-of-buffer)
   ("e" . end-of-buffer)
   ("M-a" . beginning-of-buffer-other-window)
   ("M-e" . end-of-buffer-other-window)
   ("h" . consult-outline) ;; Alternative: consult-org-heading
   ("i" . consult-imenu)
   ("M-i" . consult-imenu-multi)
   ("j" . avy-goto-line-below)
   ("M-j" . x/ace-goto-char-timer)
   ("k" . avy-goto-line-above)
   ("l" . avy-goto-line)
   ("M-l" . consult-goto-line)
   ("m" . consult-mark)
   ("M-m" . consult-global-mark)
   ("u" . jump-to-register)
   ("M-u" . point-to-register)
   ("v" . scroll-other-window)
   ("M-v" . scroll-other-window-down)))

(global-set-key (kbd "M-j") #'x/navigation-map)

(provide 'x-navigation)
;;; x-navigation.el ends here
