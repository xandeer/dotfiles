;;; x-xwidget.el --- x-xwidget -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package
 '(xwwp
   :host github :repo "xandeer/xwwp"
   ;; :branch "master"
   :files (:defaults "*.js" "*.css")
   ;; :pre-build ("make")
   ))
(require-package 'ctable)

(require 'xwwp)
(require 'xwwp-follow-link)
(require 'xwwp-history)
(setq xwidget-webkit-bookmark-jump-new-session t)

(setq xwidget-webkit-enable-plugins t)
(setq xwwp-history-filename (no-littering-expand-var-file-name "xwwp-history"))

(setq xwwp-ace-candidate-selector
      "button, input, [href], select, textarea, [tabindex]:not([tabindex=\"-1\"]), [class*=btn], [class=tab-item-content], [class=list-item]")
(require 'xwwp-ace)

(with-eval-after-load 'xwidget
  (defun x-xwidget--get-half-window-height ()
    (/ (xwidget-window-inside-pixel-height (selected-window))
       2))

  (defun x-xwidget--get-window-height-with-margin (margin)
    (- (xwidget-window-inside-pixel-height (selected-window))
       (* margin (window-font-height))))

  (defun x-xwidget--scroll-up-half ()
    (interactive)
    (xwidget-webkit-scroll-up (x-xwidget--get-half-window-height)))

  (defun x-xwidget--scroll-down-half ()
    (interactive)
    (xwidget-webkit-scroll-down (x-xwidget--get-half-window-height)))

  (defun x-xwidget--scroll-up ()
    (interactive)
    (xwidget-webkit-scroll-up (x-xwidget--get-window-height-with-margin 3)))

  (defun x-xwidget--scroll-down ()
    (interactive)
    (xwidget-webkit-scroll-down (x-xwidget--get-window-height-with-margin 3)))

  (define-key xwidget-webkit-mode-map [remap meow-next] #'x-xwidget--scroll-up)
  (define-key xwidget-webkit-mode-map [remap scroll-up] #'x-xwidget--scroll-up)

  (define-key xwidget-webkit-mode-map [remap meow-prev] #'x-xwidget--scroll-down)
  (define-key xwidget-webkit-mode-map [remap scroll-down] #'x-xwidget--scroll-down)

  (define-key xwidget-webkit-mode-map (kbd "d") #'x-xwidget--scroll-up-half)
  (define-key xwidget-webkit-mode-map (kbd "e") #'x-xwidget--scroll-down-half)
  (define-key xwidget-webkit-mode-map (kbd "d") #'x-xwidget--scroll-up)
  (define-key xwidget-webkit-mode-map (kbd "e") #'x-xwidget--scroll-down)
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

;;; stylus
(defun x/xwidget-inject-style (xs path)
  (xwwp-html-inject-style
   xs
   "x-stylus"
   (with-temp-buffer
     (insert-file-contents (expand-file-name path))
     (buffer-string))))

(defcustom x/xwidget-styles-dir
  (no-littering-expand-etc-file-name "xwidget/stylus")
  "Where to store css files.")

(defcustom x/xwidget-styles-list
  '(("https://weread.qq.com/web/reader/" . "weread.css")
    ("https://exercism.org/tracks/clojure/exercises/" . "exercism.css"))
  "Stylus list.")

(defvar-local x-xwidget--stylus-started-p nil)
(defun x/xwidget-stylus-start (xs)
  (message "Stylus start: %s" (xwidget-webkit-uri xs))
  (mapcar (lambda (i)
            (when (s-starts-with? (car i)
                                  (xwidget-webkit-uri xs))
              (message "Inject %s" (cdr i))
              (x/xwidget-inject-style xs (expand-file-name (cdr i) x/xwidget-styles-dir))))
          x/xwidget-styles-list))

(defun x/xwidget-load-changed-callback (xwidget xwidget-event-type)
  (if (not (buffer-live-p (xwidget-buffer xwidget)))
      (xwidget-log
       "error: loaded callback called for xwidget with dead buffer")
    (if (eq xwidget-event-type 'load-changed)
        (x/xwidget-stylus-start xwidget))))

(advice-add 'xwidget-webkit-callback :before #'x/xwidget-load-changed-callback)
;; (advice-remove 'xwidget-webkit-callback  #'x/xwidget-load-changed-callback)

(provide 'x-xwidget)
;;; x-xwidget.el ends here
