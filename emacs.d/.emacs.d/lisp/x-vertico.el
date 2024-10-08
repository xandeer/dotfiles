;;; x-vertico.el --- x-vertico -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/append-init-hook #'vertico-mode)

(when x/vertico-posframe?
  (require 'vertico-posframe)
  (x/append-init-hook #'vertico-posframe-mode))

(with-eval-after-load 'orderless
  ;; (setq completion-styles '(substring orderless))
  ;; (setq completion-styles '(basic partial-completion))
  (setq completion-styles '(orderless))
  ;; (add-to-list 'marginalia-prompt-categories '("Node" . roam))
  ;; (setq completion-category-defaults
  ;;       '((file (styles . (basic orderless)))
  ;;         (roam (styles . (basic orderless)))
  ;;         (buffer (styles . (basic orderless)))
  ;;         (project-files (styles . (basic orderless)))))

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

  (define-key vertico-map (kbd "(") #'vertico-previous-group)
  (define-key vertico-map (kbd ")") #'vertico-next-group))

(provide 'x-vertico)
;;; x-vertico.el ends here
