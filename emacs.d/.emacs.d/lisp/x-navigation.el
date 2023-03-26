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
 '(("a"    beginning-of-buffer)
   ("e"    end-of-buffer)
   ("d"    dumb-jump-hydra/body)
   ("M-a"  beginning-of-buffer-other-window)
   ("M-e"  end-of-buffer-other-window)
   ("h"    consult-outline) ;; Alternative: consult-org-heading
   ("i"    consult-imenu)
   ("M-i"  consult-imenu-multi)
   ("j"    avy-goto-line-below)
   ("M-j"  x/ace-goto-char-timer)
   ("k"    avy-goto-line-above)
   ("l"    avy-goto-line)
   ("M-l"  consult-goto-line)
   ("m"    consult-mark)
   ("M-m"  consult-global-mark)
   ("u"    jump-to-register)
   ("M-u"  point-to-register)
   ("v"    scroll-other-window)
   ("M-v"  scroll-other-window-down)))

(global-set-key (kbd "M-j") #'x/navigation-map)

;;; dumb-jump
(x/package-use 'dumb-jump)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(defhydra dumb-jump-hydra (:color blue :columns 3)
  "Dumb Jump"
  ("j" dumb-jump-go "Go")
  ("o" dumb-jump-go-other-window "Other window")
  ("e" dumb-jump-go-prefer-external "Go external")
  ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ("i" dumb-jump-go-prompt "Prompt")
  ("l" dumb-jump-quick-look "Quick look")
  ("b" dumb-jump-back "Back"))

(provide 'x-navigation)
;;; x-navigation.el ends here
