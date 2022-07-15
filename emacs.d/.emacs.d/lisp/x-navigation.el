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

(let ((map (define-prefix-command 'x-navigation-map)))
  (global-set-key (kbd "M-j") map)

  (define-key map "a" #'beginning-of-buffer)
  (define-key map "e" #'end-of-buffer)
  (define-key map (kbd "M-a") #'beginning-of-buffer-other-window)
  (define-key map (kbd "M-e") #'end-of-buffer-other-window)
  (define-key map "h" #'consult-outline) ;; Alternative: consult-org-heading
  (define-key map "i" #'consult-imenu)
  (define-key map (kbd "M-i") #'consult-imenu-multi)
  (define-key map "j" #'avy-goto-line-below)
  (define-key map (kbd "M-j") #'x/ace-goto-char-timer)
  (define-key map "k" #'avy-goto-line-above)
  (define-key map "l" #'avy-goto-line)
  (define-key map (kbd "M-l") #'consult-goto-line)
  (define-key map "m" #'consult-mark)
  (define-key map (kbd "M-m") #'consult-global-mark)
  (define-key map "u" #'jump-to-register)
  (define-key map (kbd "M-u") #'point-to-register)
  (define-key map "v" #'scroll-other-window)
  (define-key map (kbd "M-v") #'scroll-other-window-down))

(provide 'x-navigation)
;;; x-navigation.el ends here
