;;; x-docs.el --- docs: current use `devdocs' -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defcustom x/docs-backend 'devdocs
  "The backend to search documentation."
  :type '(choice (const devdocs)
                 (const dash)))

;;; devdocs
(defun x/docs-setup-devdocs ()
  (require 'devdocs)

  (defun x/devdocs-setup (docs)
    (lambda ()
      (setq-local devdocs-current-docs docs)))

  (add-hook 'css-mode-hook (x/devdocs-setup '("css")))
  (add-hook 'elixir-mode-hook (x/devdocs-setup '("elixir~1.13")))
  (add-hook 'kotlin-mode-hook (x/devdocs-setup '("kotlin~1.6" "openjdk~18")))
  (add-hook 'typescript-mode-hook (x/devdocs-setup '("typescript" "javascript" "rxjs")))
  (add-hook 'js2-mode-hook (x/devdocs-setup '("javascript" "rxjs")))
  (add-hook 'clojure-mode-hook (x/devdocs-setup '("clojure~1.11")))

  (with-eval-after-load 'devdocs
    (require 'shrface)
    (add-hook 'devdocs-mode-hook #'shrface-mode))

  (defun x/devdocs-lookup ()
    "Lookup the symbol at point in devdocs."
    (interactive)
    (let ((word (thing-at-point 'symbol)))
      (if word
          (devdocs-lookup nil word)
        (devdocs-lookup)))
    (select-window (get-buffer-window "*devdocs*")))

  (defalias 'x/docs-lookup #'x/devdocs-lookup))

;;; dash docs
(defun x/docs-setup-dash ()
  (require 'dash-docs)
  (require 'consult-dash)

  (setq dash-docs-docsets-path (expand-file-name "~/.cache/dash"))
  (setq dash-docs-browser-func 'xwidget-webkit-browse-url)
  (setq dash-docs-common-docsets nil)
  (setq dash-docs-enable-debugging nil)

  (defalias 'x/docs-lookup #'consult-dash)

  (with-eval-after-load 'consult
    (consult-customize x/docs-lookup :initial (thing-at-point 'symbol)))

  (defun x/dash-docs-setup (docs)
    (lambda ()
      (setq-local consult-dash-docsets docs)
      (setq-local dash-docs-docsets docs)))

  (add-hook 'css-mode-hook (x/dash-docs-setup '("CSS")))
  (add-hook 'kotlin-mode-hook (x/dash-docs-setup '("kotlin" "Java_SE17")))
  (add-hook 'typescript-mode-hook (x/dash-docs-setup '("TypeScript" "JavaScript" "RxJS")))
  (add-hook 'js2-mode-hook (x/dash-docs-setup '("JavaScript")))
  (add-hook 'clojure-mode-hook (x/dash-docs-setup '("Clojure"))))

;;; choice
(defun x/docs-switch--backend (backend)
  "Switch `x/docs-backend' to BACKEND."
  (setq x/docs-backend backend)
  (if (eq backend 'devdocs)
      (x/docs-setup-devdocs)
    (x/docs-setup-dash))
  (message "docs backend switched to %s" backend))

(defun x/docs-switch-backend ()
  "Switch backend to `devdocs' or `dash'."
  (interactive)
  (if (eq x/docs-backend 'devdocs)
      (setq x/docs-backend 'dash)
    (setq x/docs-backend 'devdocs))
  (x/docs-switch--backend x/docs-backend))

(x/append-init-hook (apply-partially #'x/docs-switch--backend x/docs-backend))

(provide 'x-docs)
;;; x-doc.el ends here
