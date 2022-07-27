;;; x-web.el --- x-web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (add-to-list 'auto-mode-alist '("\\.[tj]s\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(require 'x-javascript)
(require 'x-ts)

(defun x/tide-setup ()
  "Setup for `typescript-mode'."
  ;; Check for forbiding org-babel
  (when (or (s-ends-with? ".ts" (buffer-name))
            (s-ends-with? ".js" (buffer-name)))
    (tide-setup)
    (tide-hl-identifier-mode)

    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-tide))))

(defun x/tide-format ()
  "Format with optimizing imports."
  (interactive)
  (tide-format)
  (tide-organize-imports))

(with-eval-after-load 'tide
  (let ((map tide-mode-map))
    (define-key map (kbd "C-c C-f") #'x/tide-format)
    (define-key map (kbd "C-c r") #'tide-rename-symbol)
    (define-key map (kbd "M-k") #'tide-jump-back)
    (define-key map (kbd "M-,") #'tide-references)
    (define-key map (kbd "H-r") #'yarn-run)))

(add-hook 'typescript-mode-hook #'x/tide-setup)
(add-hook 'js2-mode-hook #'x/tide-setup)

;;; yarn
(autoload 'yarn-install "yarn" nil t)
(autoload 'yarn-test "yarn" nil t)
(with-eval-after-load 'yarn
  (require 'cl))

(provide 'x-web)
;;; x-web.el ends here
