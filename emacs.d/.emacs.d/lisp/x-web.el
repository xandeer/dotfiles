;;; x-web.el --- x-web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (add-to-list 'auto-mode-alist '("\\.[tj]s\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(require 'x-javascript)
(require 'x-typescript)

;;; tide
(defun x/tide-setup ()
  "Setup for `tide'."
  ;; Check for forbiding org-babel
  (when (and (buffer-file-name)
             (string-match-p
              (regexp-opt '(".json" ".js" ".jsx" ".ts" ".tsx"))
              (buffer-file-name)))
    (tide-setup)
    (tide-hl-identifier-mode)

    (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-tide))))

(defun x/tide-format ()
  "Format with optimizing imports."
  (interactive)
  (tide-format)
  (tide-organize-imports))

(with-eval-after-load 'tide
  (x/define-keys tide-mode-map
                 '(("C-c C-f" x/tide-format)
                   ("C-c C-r" tide-rename-symbol)
                   ("M-." lsp-ui-peek-find-definitions)
                   ("M-," lsp-ui-peek-find-references))))

(add-hook 'typescript-mode-hook #'x/tide-setup)
;; (add-hook 'js2-mode-hook #'x/tide-setup)
;; (add-hook 'json-mode-hook #'x/tide-setup)

;;; lsp
(defun x/typescript-lsp-setup ()
  "Setup `lsp' for `typescript-mode'.
Use `lsp''s definitions/references jump."
  (setq-local lsp-completion-enable nil)
  (lsp))
(add-hook 'typescript-mode-hook #'x/typescript-lsp-setup)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'json-mode-hook #'lsp)

;;; bun
(with-eval-after-load 'bun
  (require 'cl))

(defun x/web-locate-package-json (&optional _dir)
  "Locate `package.json` file."
  (let ((dir (or _dir default-directory)))
    (locate-dominating-file dir "package.json")))

(defvar x/web-mode-map
  (let ((map (make-sparse-keymap)))
    (x/define-keys
     map
     '(("C-c d" x/docs-lookup)
       ("C-c i" bun-install)
       ("C-c r" bun-run)
       ("C-c t" bun-test)))
    map))

(define-minor-mode x/web-mode
  "Minor mode for web development."
  :keymap x/web-mode-map
  :group 'x/web
  (if x/web-mode
      (progn
        ;; (require 'yarn)
        (require 'bun))))

(defun x/web-mode-on ()
  "Turn on `x/web-mode'."
  (interactive)
  (x/web-mode 1))

(defun x/web-mode-off ()
  "Turn off `x/web-mode'."
  (interactive)
  (x/web-mode -1))

(defun x/web-mode-auto ()
  "Turn on `x/web-mode' if the current file is a web file."
  (when (x/web-locate-package-json)
    (x/web-mode)))

(add-hook 'find-file-hook #'x/web-mode-auto)

(provide 'x-web)
;;; x-web.el ends here
