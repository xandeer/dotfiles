;;; init-company.el --- Completion with company -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf company
  :straight t
  :hook (after-init-hook . global-company-mode)
  :init
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  :config
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))
  (setq company-minimum-prefix-length 1)
  (setq-default company-dabbrev-code-other-buffers 'all
                company-idle-delay .1
                company-tooltip-align-annotations t))
 ;; (diminish 'company-mode))

(provide 'init-company)
;;; init-company.el ends here
