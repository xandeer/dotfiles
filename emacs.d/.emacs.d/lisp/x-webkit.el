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

(setq xwidget-webkit-bookmark-jump-new-session t)

(setq xwwp-history-filename (no-littering-expand-var-file-name "xwwp-history"))

(with-eval-after-load 'xwidget
  (defun x-xwidget--get-half-window-height ()
    (/ (xwidget-window-inside-pixel-height (selected-window))
       2))

  (defun x-xwidget--scroll-up-half ()
    (interactive)
    (xwidget-webkit-scroll-up (x-xwidget--get-half-window-height)))

  (defun x-xwidget--scroll-down-half ()
    (interactive)
    (xwidget-webkit-scroll-down (x-xwidget--get-half-window-height)))

  (define-key xwidget-webkit-mode-map [remap meow-next] #'xwidget-webkit-scroll-up)
  (define-key xwidget-webkit-mode-map [remap scroll-up] #'xwidget-webkit-scroll-up)
  
  (define-key xwidget-webkit-mode-map [remap meow-prev] #'xwidget-webkit-scroll-down)
  (define-key xwidget-webkit-mode-map [remap scroll-down] #'xwidget-webkit-scroll-down)

  (define-key xwidget-webkit-mode-map (kbd "d") #'x-xwidget--scroll-up-half)
  (define-key xwidget-webkit-mode-map (kbd "e") #'x-xwidget--scroll-down-half)
  (define-key xwidget-webkit-mode-map (kbd "f") #'xwwp-ace-toggle)
  (define-key xwidget-webkit-mode-map (kbd "v") #'xwwp-follow-link))

(defun x/xwidget-browse (url)
  "Ask xwidget-webkit to browse URL in a new session."
  (interactive "sUrl: ")
  (xwidget-webkit-browse-url url t))

(defun x/xwidget-search-clojure-core (key)
  "Use xwidget-webkit search KEY from clojure.core docs."
  (interactive "sKey: ")
  (xwidget-webkit-browse-url (concat "https://clojuredocs.org/clojure.core/" key) t))

(provide 'x-webkit)
;;; x-webkit.el ends here
