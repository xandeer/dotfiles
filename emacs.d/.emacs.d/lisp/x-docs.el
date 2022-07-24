;;; x-docs.el --- docs: current use `dash-docs' -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq dash-docs-docsets-path (expand-file-name "~/syncthing/personal/docsets"))
(setq dash-docs-browser-func 'xwidget-webkit-browse-url)
(setq dash-docs-common-docsets nil)
(setq dash-docs-enable-debugging nil)

(defalias 'x/docs-lookup #'consult-dash)

(with-eval-after-load 'consult
  (consult-customize x/docs-lookup :initial (thing-at-point 'symbol)))

(defun x/docs-setup (docs)
  (lambda ()
    (setq-local consult-dash-docsets docs)
    (setq-local dash-docs-docsets docs)))

(add-hook 'css-mode-hook (x/docs-setup '("CSS")))
(add-hook 'kotlin-mode-hook (x/docs-setup '("kotlin" "Java_SE17")))
(add-hook 'typescript-mode-hook (x/docs-setup '("TypeScript" "JavaScript" "RxJS")))
(add-hook 'js2-mode-hook (x/docs-setup '("JavaScript")))
(add-hook 'clojure-mode-hook (x/docs-setup '("Clojure")))

(provide 'x-docs)
;;; x-doc.el ends here
