;;; x-elixir.el --- x-elixir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'elixir-mode)
(require-package 'alchemist)
(require-package 'exunit)
(require-package 'flycheck-credo)

(with-eval-after-load 'smartparens-mode
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p))))


(with-eval-after-load 'elixir-mode
  (flycheck-credo-setup))

(add-hook 'elixir-mode-hook #'alchemist-mode)
(setq alchemist-execute-command (x/prepend-homebrew-path "elixir"))
(setq alchemist-compile-command (x/prepend-homebrew-path "elixirc"))
;; (setq alchemist-iex-program-name (x/prepend-homebrew-path "iex"))
(setq alchemist-mix-command (x/prepend-homebrew-path "mix"))
(setq alchemist-mix-env "prod")

(defun x/elixir--completion-setup ()
  "Setup elixir completion for with cape."
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'alchemist-company))))

(add-hook 'alchemist-mode-hook #'x/elixir--completion-setup)
(add-hook 'alchemist-iex-mode-hook #'x/elixir--completion-setup)

(with-eval-after-load 'alchemist-iex
  (define-key alchemist-iex-mode-map (kbd "TAB") #'corfu-complete))

(provide 'x-elixir)
;;; x-elixir.el ends here
