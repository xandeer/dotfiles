;;; x-company.el --- Completion with company -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require-package 'company)
;; (x/append-init-hook #'global-company-mode)

(with-eval-after-load 'company
  (setq company-idle-delay 0.2)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case 'keep)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-code-other-buffers 'all)

  (define-key company-active-map (kbd "M-s") #'company-filter-candidates)
  (define-key company-active-map (kbd "C-s") #'counsel-company))

(provide 'x-company)
;;; x-company.el ends here
