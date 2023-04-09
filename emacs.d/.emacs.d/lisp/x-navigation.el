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

(transient-define-prefix x/transient-navigate ()
  "Navigate."
  [["Mark & Register"
    ("m"    "Mark" consult-mark)
    ("M-m"  "Global mark" consult-global-mark)
    ("u"    "Jump to register" jump-to-register)
    ("M-u"  "Point to register" point-to-register)]
   ["Line"
    ("j"    "Goto line below" avy-goto-line-below)
    ("k"    "Goto line above" avy-goto-line-above)
    ("l"    "Goto line" avy-goto-line)
    ("M-l"  "Goto line" consult-goto-line)]
   ["Buffer"
    ("a"    "Beginning of buffer" beginning-of-buffer)
    ("e"    "End of buffer" end-of-buffer)
    ("M-a"  "Beginning of buffer other window" beginning-of-buffer-other-window)
    ("M-e"  "End of buffer other window" end-of-buffer-other-window)
    ("v"    "Scroll other window" scroll-other-window)
    ("M-v"  "Scroll other window down" scroll-other-window-down)]
   ["Other"
    ("d"    "Dumb jump" x/transient-dumb-jump)
    ("h"    "Outline" consult-outline) ;; Alternative: consult-org-heading
    ("i"    "Imenu" consult-imenu)
    ("M-i"  "Imenu multi" consult-imenu-multi)
    ("M-j"  "Goto char" x/ace-goto-char-timer)]])

(global-set-key (kbd "M-j") #'x/transient-navigate)

;;; dumb-jump
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(define-transient-command x/transient-dumb-jump ()
  "Transient for Dumb Jump."
  [["Dumb Jump"
    ("j" "Go" dumb-jump-go)
    ("o" "Other window" dumb-jump-go-other-window)
    ("e" "Go external" dumb-jump-go-prefer-external)
    ("x" "Go external other window" dumb-jump-go-prefer-external-other-window)
    ("i" "Prompt" dumb-jump-go-prompt)
    ("l" "Quick look" dumb-jump-quick-look)
    ("b" "Back" dumb-jump-back)]])

(provide 'x-navigation)
;;; x-navigation.el ends here
