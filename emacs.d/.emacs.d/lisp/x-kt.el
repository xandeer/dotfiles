;;; x-kt.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq kotlin-tab-width 2)
(add-to-list 'exec-path
             (expand-file-name
              "~/projects/others/kotlin-language-server/server/build/install/server/bin"))
(require-package 'flycheck-kotlin)
(require-package 'kotlin-mode)
(add-hook 'kotlin-mode-hook #'flycheck-kotlin-setup)

(require-package
 '(ob-kotlin :host github
             :repo "zweifisch/ob-kotlin"))
(autoload #'org-babel-execute:kotlin "ob-kotlin" nil t)
(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages
               '(kotlin . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-babel-tangle-lang-exts
               '("kotlin" . "kt")))
;;; x-point
(defvar x--kotlin-left "[([{]"
  "Opening delimiter.")

(defvar x--kotlin-right "[])}]"
  "Closing delimiter.")

(defsubst x--kotlin-right-p ()
  (looking-back x--kotlin-right
                (line-beginning-position)))

(defsubst x--kotlin-left-p ()
  (looking-at x--kotlin-left))

(defun x--kotlin-different ()
  (interactive)
  (cond ((x--kotlin-right-p)
         (sp-backward-sexp))
        ((x--kotlin-left-p)
         (sp-forward-sexp))
        ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))))

;;; x-point
(with-eval-after-load 'x-point-mode
  (setq x-point-kotlin-speed-commands
        '(("Navigation")
          ;; ("j" . lispy-down)
          ;; ("k" . lispy-up)
          ;; ("f" . lispy-flow)
          ;; ("b" . lispy-back)
          ;; ("u" . lispy-undo)
          ("d" . x--kotlin-different)
          ;; ("l" . lispy-right)
          ;; ("h" . x-hydra-hideshow/body)
          ))

  (defun x-point-kotlin-speed-command-activate (keys)
    "Hook for activating single-letter speed commands.
See `x-point-kotlin-speed-commands' for configuring them."
    (when (and (equal major-mode 'kotlin-mode)
               (x-point-bol-p))
      (back-to-indentation))
    (when (and (equal major-mode 'kotlin-mode)
               (or (region-active-p)
                   (x--kotlin-left-p)
                   (x--kotlin-right-p)))
      (cdr (assoc keys (append x-point-kotlin-speed-commands
                               x-point-speed-commands)))))

  (add-hook 'x-point-speed-command-hook #'x-point-kotlin-speed-command-activate -90))

(provide 'x-kt)
;;; x-kt.el ends here
