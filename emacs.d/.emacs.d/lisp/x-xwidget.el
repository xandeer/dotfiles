;;; x-xwidget.el --- x-xwidget -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; :pre-build ("make")
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
  (setq kill-buffer-query-functions
        (delq 'xwidget-kill-buffer-query-function kill-buffer-query-functions))
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

  (defun x-xwidget--eww ()
    (interactive)
    (let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
      (when url (eww url))))

  (defun x-xwidget--browse-generic ()
    (interactive)
    (let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
      (when url (browse-url-generic url))))

  (x/define-keys
   xwidget-webkit-mode-map
   '(([remap meow-next] x-xwidget--scroll-up)
     ([remap scroll-up] x-xwidget--scroll-up)

     ([remap meow-prev] x-xwidget--scroll-down)
     ([remap scroll-down] x-xwidget--scroll-down)

     ;; ("d" x-xwidget--scroll-up-half)
     ;; ("e" x-xwidget--scroll-down-half)
     ("d" x-xwidget--scroll-up)
     ("j" x-xwidget--scroll-up)
     ("e" x-xwidget--scroll-down)
     ("k" x-xwidget--scroll-down)
     ("f" xwwp-ace-toggle)
     ("v" xwwp-follow-link)
     ("x" kill-current-buffer)
     ("o" x-xwidget--browse-generic)
     ("E" x-xwidget--eww)
     ("K" xwidget-webkit-back)
     ;; todo: doesn't work in the input field
     ("<escape>" keyboard-quit))))

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
