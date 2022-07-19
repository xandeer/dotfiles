;;; x-web.el --- x-web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist
             '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("\\.vue\\'" . web-mode))

;; (require-package 'counsel-css)
;; (add-hook 'css-mode-hook #'counsel-css-imenu-setup)

(add-to-list 'auto-mode-alist
             '("\\.js\\'" . js-mode))
(add-hook 'js-mode-hook #'lsp)

(with-eval-after-load 'js-mode
  ;; fix `js-find-symbol' [M-.] overriding other packages' keybinding.
  (substitute-key-definition 'js-find-symbol 'xref-find-definitions js-mode-map))

(unless
    (fboundp 'org-babel-execute:js)
  (autoload #'org-babel-execute:js "ob-js" nil t))

(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
               '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts
               '("js" . "js"))
  ;; (add-to-list 'org-babel-default-header-args:js
  ;;             '(:results . "output"))
             )

(with-eval-after-load 'typescript-mode
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook #'tide-hl-identifier-mode)
  ;; (add-hook 'before-save-hook #'tide-format-before-save)
  )
(with-eval-after-load 'tide
  (define-key tide-mode-map (kbd "C-x f") #'tide-format)
  (define-key tide-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
  (define-key tide-mode-map (kbd "M-,") #'lsp-ui-peek-find-references))

;;; yarn
(autoload 'yarn-install "yarn" nil t)
(autoload 'yarn-test "yarn" nil t)

;;; repl
(setq skewer-bower-cache-dir (no-littering-expand-var-file-name "skewer-cache"))
;; yarn add global typescript ts-node
;; for console
;; yarn add global @types/node
(setq ts-comint-program-command "ts-node")
(setq ts-comint-program-arguments '("--skipProject"))

(defun x/web-kill-ts-repl ()
  "Kill the typescript REPL process."
  (interactive)
  (let ((process (get-buffer-process ts-comint-buffer)))
    (when process
      (kill-process process))))

(defun x/web--setup-ts-repl ()
  "Setup ts-repl."
  (local-set-key (kbd "C-x C-e") #'ts-send-last-sexp)
  (local-set-key (kbd "C-c C-b") #'ts-send-buffer)
  (local-set-key (kbd "C-c C-z") #'run-ts)
  (local-set-key (kbd "C-c C-k") #'x/web-kill-ts-repl))
(add-hook 'typescript-mode-hook #'x/web--setup-ts-repl)

;;; exercism typescript
(with-eval-after-load 'typescript-mode
  (defun x/web-unskip-test ()
    "Unskip test in current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "xit" nil t)
        (replace-match "it"))))

  (defhydra x/hydra-typescript (:exit t :columns 4 :idle 0.3)
    "
Typescript\n"
    ("d" x/devdocs-lookup "devdocs lookup at point")
    ("r" x/exercism-open-readme-other-window "open readme in other window")
    ("i" x/web-unskip-test "unskip test")
    ("t" yarn-test "yarn test")
    ("u" x/exercism-submit "submit to exercism"))

  (define-key typescript-mode-map (kbd "H-k") #'x/hydra-typescript/body))

(provide 'x-web)
;;; x-web.el ends here
