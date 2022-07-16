;;; x-elixir.el --- x-elixir -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'smartparens-mode
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p))))

(defun x/elixir-exercism-clear ()
  "Delete useless comments."
  (interactive)
  (x/replace "# Please implement .*" ""))

(with-eval-after-load 'elixir-mode
  (flycheck-credo-setup)
  (define-key elixir-mode-map (kbd "C-x f") #'elixir-format)
  (define-key elixir-mode-map (kbd "C-c t n") #'alchemist-project-run-tests-for-current-file)
  (define-key elixir-mode-map (kbd "C-c t o") #'alchemist-project-toggle-file-and-tests-other-window)
  (define-key elixir-mode-map (kbd "C-c t i") #'x/elixir-exercism-clear)
  (define-key elixir-mode-map (kbd "C-c e r") #'x/exercism-open-readme-other-window)
  (define-key elixir-mode-map (kbd "C-c e u") #'x/exercism-submit))

(add-hook 'elixir-mode-hook #'alchemist-mode)
(setq alchemist-execute-command (x/prepend-homebrew-path "elixir"))
(setq alchemist-compile-command (x/prepend-homebrew-path "elixirc"))
;; (setq alchemist-iex-program-name (x/prepend-homebrew-path "iex"))
(setq alchemist-mix-command (x/prepend-homebrew-path "mix"))
(setq alchemist-mix-env "prod")

;; (add-to-list 'exec-path (expand-file-name ".lsp/elixir" "~"))
(setq lsp-elixir-ls-server-dir (expand-file-name ".lsp/elixir" "~"))
(setq lsp-elixir-local-server-command (expand-file-name ".lsp/elixir/language_server.sh" "~"))
(defun x/elixir--setup-lsp ()
  (setq-local lsp-elixir-project-dir
              (expand-file-name (locate-dominating-file (buffer-file-name) "mix.exs"))))
(add-hook 'elixir-mode-hook #'x/elixir--setup-lsp)

(defun x/elixir--completion-setup ()
  "Setup elixir completion for with cape."
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'alchemist-company))))

;; (add-hook 'alchemist-mode-hook #'x/elixir--completion-setup)
;; (add-hook 'alchemist-iex-mode-hook #'x/elixir--completion-setup)

;; Alchemist doesn't use hook symbols to add these backends, so we have to use
;; the entire closure to get rid of it.
(let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
  (remove-hook 'alchemist-mode-hook fn)
  (remove-hook 'alchemist-iex-mode-hook fn))

(with-eval-after-load 'alchemist-iex
  (define-key alchemist-iex-mode-map (kbd "TAB") #'corfu-complete))

(provide 'x-elixir)
;;; x-elixir.el ends here
