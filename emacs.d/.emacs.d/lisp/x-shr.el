;;; x-shr.el --- shr related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; link hint
(defun x/link-hint-open-in-current-window ()
  "Call `link-hint-open-link' in the current window."
  (interactive)
  (let ((avy-all-windows nil))
    (link-hint-open-link)))

(defvar-local x/shr-next-document-fn nil
  "Function to call when `shr-next-document' is called.")

(defvar-local x/shr-previous-document-fn nil
  "Function to call when `shr-previous-document' is called.")

(defun x/shr-scroll-up ()
  "Scroll up or goto next document."
  (interactive)
  (condition-case nil
      (scroll-up-command)
    (end-of-buffer
     (when x/shr-next-document-fn
       (funcall x/shr-next-document-fn)))))

(defun x/shr-scroll-down ()
  "Scroll down or goto previous document."
  (interactive)
  (condition-case nil
      (scroll-down-command)
    (beginning-of-buffer
     (when x/shr-previous-document-fn
       (funcall x/shr-previous-document-fn)
       (end-of-buffer)))))

;;; shr
(defconst x/shr-map '(("b" Info-history-back)
                      ("d" x/shr-scroll-up)
                      ("j" x/shr-scroll-up)
                      ("e" x/shr-scroll-down)
                      ("f" x/link-hint-open-in-current-window)
                      ("i" consult-imenu)
                      ("s" consult-line)
                      ("l" sdcv-search-pointer)
                      ("q" previous-buffer)))

(with-eval-after-load 'shr
  (set-face-foreground 'shr-code "#7bc275")
  (x/define-keys shr-map x/shr-map))

;;; shrface
(with-eval-after-load 'shrface
  (setq shrface-hrefshversatile t)

  (set-face-attribute 'variable-pitch nil :font (format "%s:pixelsize=%d" "Bookerly" 15))
  (set-face-foreground 'shrface-verbatim "#f00056")

  (add-hook 'shrface-mode-hook (lambda ()
                                 (org-indent-mode -1)))

  (x/define-keys shrface-mode-map x/shr-map)
  (x/define-keys shrface-mode-map '(("M-n"     shrface-next-headline)
                                    ("M-p"     shrface-previous-headline)
                                    ("TAB"     shrface-outline-cycle)
                                    ("S-<tab>" shrface-outline-cycle-buffer)))

  ;; shr-tag-pre-highlight
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))

  ;; https://github.com/chenyanming/shrface#hacking-the-shr-tag-pre-highlightel
  (add-to-list 'shr-external-rendering-functions '(pre . shrface-shr-tag-pre-highlight))
  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     "kotlin"       ; detection always wrong
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat "#+begin_src " lang) 'face nil 'display '(space :width (1)))
       "\n"
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       "\n"
       (propertize "#+end_src" 'face nil 'display '(space :width (1))))
      (shr-ensure-newline)
      (setq end (point))
      (if (x/theme-light-p)             ; light theme
          (add-face-text-property start end '(:background "#ede5c3" :extend t))
        (add-face-text-property start end '(:background "#191b1e" :extend t)))
      (shr-ensure-newline)
      (insert "\n"))))

;;; eww
(add-hook 'eww-after-render-hook #'shrface-mode)
(with-eval-after-load 'eww
  (x/define-keys eww-mode-map x/shr-map))
(remove-hook 'eww-after-render-hook #'shrface-mode)

;;; info
(with-eval-after-load 'info
  (x/define-keys Info-mode-map x/shr-map))

;;; helpful
(with-eval-after-load 'helpful
  (x/define-keys helpful-mode-map x/shr-map))

;;; nov
(with-eval-after-load 'nov
  (require 'shrface)
  (add-hook 'nov-mode-hook #'shrface-mode)
  (setq nov-save-place-file nil)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

  (defun x/nov-setup ()
    (setq-local x/shr-next-document-fn #'nov-next-document)
    (setq-local x/shr-previous-document-fn #'nov-previous-document))

  (add-hook 'nov-mode-hook #'x/nov-setup)
  (x/define-keys nov-mode-map x/shr-map)

  (with-eval-after-load 'nov-xwidget
    (x/define-keys nov-mode-map '(("V" nov-xwidget-view)))))

(x/package-use '(nov-xwidget . "chenyanming/nov-xwidget"))
(require 'nov-xwidget)
(x/define-keys nov-xwidget-webkit-mode-map
               '(("n" nov-xwidget-next-document)
                 ("p" nov-xwidget-previous-document)))

(provide 'x-shr)
;;; x-shr.el ends here
