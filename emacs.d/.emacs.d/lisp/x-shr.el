;;; x-shr.el --- shr related -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; link hint
(defun x/link-hint-open-in-current-window ()
  "Call `link-hint-open-link' in the current window."
  (interactive)
  (let ((avy-all-windows nil))
    (link-hint-open-link)))

;;; shr
(defconst x/shr-map '(("b" . Info-history-back)
                      ("d" . scroll-up)
                      ("e" . scroll-down)
                      ("f" . x/link-hint-open-in-current-window)
                      ("i" . consult-imenu)
                      ("s" . consult-line)
                      ("l" . sdcv-search-pointer)))

(with-eval-after-load 'shr
  (x/define-keys shr-map x/shr-map))

;;; shrface
(with-eval-after-load 'shrface
  (setq shrface-hrefshversatile t)

  (set-face-attribute 'variable-pitch nil :font (format "%s:pixelsize=%d" "Bookerly" 15))

  (add-hook 'shrface-mode-hook (lambda ()
                                 (org-indent-mode -1)))

  (x/define-keys shrface-mode-map x/shr-map)
  (x/define-keys shrface-mode-map '(("M-n"     . shrface-next-headline)
                                    ("M-p"     . shrface-previous-headline)
                                    ("TAB"     . shrface-outline-cycle)
                                    ("S-<tab>" . shrface-outline-cycle-buffer)))

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
                     "typescript"       ; detection always wrong
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       (propertize (concat  "#+begin_src " lang) 'face nil 'display '(space :width (1)))
       "\n"
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
           code)
       "\n"
       (propertize "#+end_src" 'face nil 'display '(space :width (1))))
      (shr-ensure-newline)
      (setq end (point))
      (if nil                           ; light theme
          (add-face-text-property start end '(:background "#D8DEE9" :extend t))
        (add-face-text-property start end '(:background "#191b1e" :extend t)))
      (shr-ensure-newline)
      (insert "\n"))))

;;; eww
(add-hook 'eww-after-render-hook #'shrface-mode)

;;; info
(with-eval-after-load 'info
  (x/define-keys Info-mode-map x/shr-map))

;;; nov
(with-eval-after-load 'nov
  (require 'shrface)
  (add-hook 'nov-mode-hook #'shrface-mode)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; (x/package-use '(nov-xwidget . "chenyanming/nov-xwidget"))
;; (require 'nov-xwidget)

(provide 'x-shr)
;;; x-shr.el ends here
