;;; init-company.el --- Completion with company -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf company
  :straight t
  :hook (after-init-hook . global-company-mode)
  :bind
  (:company-active-map
   ("M-s" . company-filter-candidates)
   ("C-s" . counsel-company))
  :custom
  (company-idle-delay                 . .2)
  (company-show-numbers               . t)
  (company-dabbrev-downcase           . nil)
  (company-dabbrev-ignore-case        . 'keep)
  (company-minimum-prefix-length      . 1)
  (company-dabbrev-code-ignore-case   . t)
  (company-tooltip-align-annotations  . t)
  (company-dabbrev-code-other-buffers . 'all))

(provide 'init-company)
;;; init-company.el ends here
