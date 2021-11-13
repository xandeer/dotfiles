;;; init-vertico.el --- init-vertico -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'vertico)
(require-package 'orderless)
(add-hook 'after-init-hook 'vertico-mode)

(with-eval-after-load 'orderless
  (setq completion-styles '(substring orderless))

  (with-eval-after-load 'pinyinlib
    (defun completion--regex-pinyin (str)
      (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)))

(with-eval-after-load 'vertico
  (require 'orderless)
  (setq vertico-cycle t)

  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  ;; (setq vertico--highlight-function #'orderless-highlight-matches)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (define-key vertico-map (kbd "C-(") #'vertico-previous-group)
  (define-key vertico-map (kbd "C-)") #'vertico-next-group))

(provide 'init-vertico)
;;; init-vertico.el ends here
