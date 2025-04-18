;;; x-prog.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
(with-eval-after-load 'eldoc-box
  (set-face-background 'eldoc-box-border "gray10"))

(add-hook 'prog-mode-hook #'color-identifiers-mode)

(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(setq highlight-indent-guides-responsive nil)
(setq highlight-indent-guides-delay 0.5)
(setq highlight-indent-guides-auto-odd-face-perc 3)
(setq highlight-indent-guides-auto-even-face-perc 6)

(x/append-init-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)
(add-hook 'org-mode-hook #'rainbow-mode)
(add-hook 'css-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

(add-hook 'css-mode-hook #'rainbow-identifiers-mode)
(add-hook 'html-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-src-mode-hook #'rainbow-delimiters-mode)

(setq htmlize-pre-style t)

(x/append-init-hook #'show-paren-mode)

(x/append-init-hook #'smartparens-global-mode)
(setq sp-hybrid-kill-entire-symbol nil)
;; https://stackoverflow.com/questions/22107501/set-emacs-to-smart-auto-line-after-a-parentheses-pair
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (sp-with-modes '(js-mode json-mode web-mode kotlin-mode css-mode typescript-mode swift-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "[" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(custom-set-faces
 '(quick-peek-border-face
   ((t
     (:background "#75b79e" :height 0.1))))
 '(quick-peek-padding-face
   ((t
     (:height 0.1)))))

(provide 'x-prog)
;;; x-prog.el ends here
