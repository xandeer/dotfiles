;;; init-company.el --- Completion with company -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf company
  :straight t
  :hook (after-init-hook . global-company-mode)
  :init
  (setq tab-always-indent 'complete)
  (add-to-list 'completion-styles 'initials t)
  :bind
  (:company-active-map
   ("C-o"        . company-search-kill-others)
   ("C-n"        . company-select-next)
   ("C-p"        . company-select-previous)
   ("C-h"        . company-quickhelp-manual-begin)
   ("C-S-h"      . company-show-doc-buffer)
   ("M-s"        . company-filter-candidates)
   ("C-s"        . counsel-company)
   ([C-tab]      . xr/company-complete)
   ([tab]        . company-complete-common-or-cycle)
   ([backtab]    . company-select-previous))
  (:company-search-map
   ("C-n"        . company-search-repeat-forward)
   ("C-p"        . company-search-repeat-backward))
  :custom
  (company-dabbrev-downcase         . nil)
  (company-dabbrev-ignore-case      . 'keep)
  (company-dabbrev-code-ignore-case . t)
  :config
  (defun xr/company-short ()
    (company-search-abort)
    (company-filter-candidates))
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))
  (setq company-minimum-prefix-length 1)
  (setq-default company-dabbrev-code-other-buffers 'all
                company-idle-delay .1
                company-tooltip-align-annotations t))
;; (diminish 'company-mode))

(provide 'init-company)
;;; init-company.el ends here
