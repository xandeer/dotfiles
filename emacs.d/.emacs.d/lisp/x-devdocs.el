;;; x-devdocs.el --- x-devdocs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'thingatpt)

(defun x/devdocs--setup (docs)
  (lambda () (setq-local devdocs-current-docs docs)))

(add-hook 'css-mode-hook (x/devdocs--setup '("css")))
(add-hook 'elixir-mode-hook (x/devdocs--setup '("elixir~1.13")))
(add-hook 'kotlin-mode-hook (x/devdocs--setup '("kotlin~1.6" "openjdk~18")))
(add-hook 'typescript-mode-hook (x/devdocs--setup '("typescript" "javascript")))
(add-hook 'clojure-mode-hook (x/devdocs--setup '("clojure~1.11")))

(defun x/devdocs-lookup ()
  "Lookup the symbol at point in devdocs."
  (interactive)
  (let ((word (thing-at-point 'symbol)))
    (if word
        (devdocs-lookup nil word)
      (devdocs-lookup))))

(provide 'x-devdocs)
;;; x-devdocs.el ends here
