;;; x-kt.el --- kotlin -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
(with-eval-after-load 'x-point-mode
  (setq x-point-kotlin-speed-special-commands
        '(("Navigation")
          ;; ("j" . lispy-down)
          ;; ("k" . lispy-up)
          ;; ("f" . lispy-flow)
          ;; ("b" . lispy-back)
          ))
  (setq x-point-kotlin-speed-commands
        '(("Navigation")
          ;; ("l" . lispy-right)
          ("Misc")
          ;; ("n" . sp-narrow-to-sexp)
          ))

  (defun x-point-kotlin-speed-command-activate (keys)
    "Hook for activating single-letter speed commands.
See `x-point-kotlin-speed-commands' for configuring them."
    (when (and (equal major-mode 'kotlin-mode)
               (x-point-bol-p))
      (back-to-indentation))
    (when (equal major-mode 'kotlin-mode)
      (cond
       ((or (region-active-p)
            (x-point-left-sexp-p)
            (x-point-right-sexp-p))
        (cdr (assoc keys (append x-point-kotlin-speed-special-commands
                                 x-point-speed-commands))))
       ((or (x-point-bol-p))
        (cdr (assoc keys (append x-point-kotlin-speed-commands
                                 x-point-speed-commands)))))))
  (defun x-point-kotlin-setup ()
    (setq-local x-point-left "[([{<]")
    (setq-local x-point-right "[])}>]"))

  (add-hook 'kotlin-mode-hook #'x-point-kotlin-setup)

  (add-hook 'x-point-speed-command-hook #'x-point-kotlin-speed-command-activate -90))

(provide 'x-kt)
;;; x-kt.el ends here
