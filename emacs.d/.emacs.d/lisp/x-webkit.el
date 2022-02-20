;;; x-webkit.el --- x-webkit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package
 '(xwwp
   :host github :repo "BlueFlo0d/xwwp"
   :branch "master"
   :files (:defaults "*.js" "*.css")
   ;; :pre-build ("make")
   ))
(require-package 'ctable)

(require 'cl)
(require 'xwwp)
(require 'xwwp-follow-link)
(require 'xwwp-ace)
(require 'xwwp-history)

(setq xwwp-history-filename (no-littering-expand-var-file-name "xwwp-history"))

(with-eval-after-load 'xwidget
  (define-key xwidget-webkit-mode-map (kbd "d") #'xwidget-webkit-scroll-up)
  (define-key xwidget-webkit-mode-map (kbd "e") #'xwidget-webkit-scroll-down)
  (define-key xwidget-webkit-mode-map (kbd "f") #'xwwp-ace-toggle)
  (define-key xwidget-webkit-mode-map (kbd "v") #'xwwp-follow-link))

(defun x-webkit-search-clojure-core (key)
  "Use xwidget-webkit search KEY from clojure.core docs."
  (interactive "sKey: ")
  (xwidget-webkit-browse-url (concat "https://clojuredocs.org/clojure.core/" key)))

(provide 'x-webkit)
;;; x-webkit.el ends here
