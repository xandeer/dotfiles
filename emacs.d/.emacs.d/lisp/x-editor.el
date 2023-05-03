;;; x-editor.el --- x-editor -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; case
(defmacro x/defun-backward (fn)
  "Create a new interactive function that call FN with a negative prefix argument.
The new function's name is derived from FN by appending \"-backward\" and
adding the \"x/\" prefix.

Example usage:
  (x/defun-backward upcase-word)

This will create a new function `x/upcase-word-backward` that calls  `upcase-word` with a negative prefix argument."
  ;; Create a new function name by appending "-backward" to the input function name
  ;; and adding the "x/" prefix.
  (let ((backward-fn (intern (concat "x/" (symbol-name fn) "-backward"))))
    ;; Define a new function with the new function name.
    `(defun ,backward-fn ()
       (interactive)
       (funcall-interactively #',fn -1))))

(define-prefix-command 'x/case-map)
(x/define-keys x/case-map
               `(("u" upcase-region)
                 ("l" downcase-region)
                 ("c" upcase-initials-region)
                 ("t" titlecase-line)
                 ("M-u" ,(x/defun-backward upcase-word))
                 ("M-l" ,(x/defun-backward downcase-word))
                 ("M-c" ,(x/defun-backward capitalize-word))
                 ("s" jinx-correct)))

(x/define-keys ctl-x-map
               '(("c" x/case-map)
                 ("." repeat)))

(unless (boundp 'x/meta-h-map)
  (define-prefix-command 'x/meta-h-map))

(x/define-keys
 x/meta-h-map
 `(("M-u" ,(x/defun-backward upcase-word))
   ("M-l" ,(x/defun-backward downcase-word))
   ("M-c" ,(x/defun-backward capitalize-word))
   ("k" x/zap-to-char)
   ("M-k" x/zap-to-char-backward)))

(defun x/zap-to-char-backward ()
  "Call `x/zap-to-char' with a negative prefix argument to zap backward."
  (interactive)
  (let ((current-prefix-arg -1))
    (call-interactively #'x/zap-to-char)))

(x/define-keys global-map
               '(("M-h" x/meta-h-map)))

(with-eval-after-load 'org
  (x/define-keys org-mode-map
                 '(("M-h" x/meta-h-map))))

;;; new line
(x/define-keys global-map
               '(("C-o" x/new-line-before)
                 ("M-o" x/new-line-after)))
;;; transpose
(define-prefix-command 'x/transpose-map)
(x/define-keys x/transpose-map
               '(("w" transpose-words)
                 ("l" transpose-lines)
                 ("s" transpose-sexps)
                 ("p" transpose-paragraphs)
                 ("t" transpose-sentences)
                 ("C-t" transpose-chars)))

(x/define-keys global-map
               '(("C-t" x/transpose-map)))

;; https://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun x/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line`.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")                 ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(x/define-keys global-map
               '(([remap move-beginning-of-line] x/smart-beginning-of-line)
                 ([remap newline] newline-and-indent)
                 ("M-;" comment-line)
                 ("H-z" undo)))

(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)

;; Delete multiple consecutive blank characters at once
(add-hook 'activate-mark-hook (lambda () (hungry-delete-mode -1)))
(add-hook 'deactivate-mark-hook #'hungry-delete-mode)
(x/append-init-hook #'global-hungry-delete-mode)

;; Something wrong with `golden-ration-mode'.
;; (setq-default visual-fill-column-width 76)
;; (x/append-init-hook #'global-visual-fill-column-mode)

(x/define-keys global-map
               '(([remap kill-ring-save] easy-kill)
                 ([remap mark-sexp] easy-mark)))

(defun x/init-whitespace-mode ()
  "Initialize `whitespace-mode'."
  (setq whitespace-style '(newline newline-mark))
  (global-whitespace-mode 1)
  (set-face-foreground 'whitespace-newline "gray75")
  (set-face-attribute 'whitespace-newline nil :height 0.7))

(run-with-idle-timer 1 nil 'x/init-whitespace-mode)

(unless
    (fboundp 'auto-compression-mode)
  (autoload #'auto-compression-mode "jka-cmpr" nil t))
(x/append-init-hook #'auto-compression-mode)

(require 'auto-save)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
(setq auto-save-idle 0.5)
(add-hook 'org-capture-mode-hook #'auto-save-disable)
(add-hook 'org-capture-prepare-finalize-hook #'auto-save-enable)
(x/append-init-hook #'auto-save-enable)

(add-hook 'eldoc-mode-hook #'eldoc-box-hover-at-point-mode)
(with-eval-after-load 'eldoc-box
  (set-face-background 'eldoc-box-border "gray10"))

(setq expand-region-subword-enabled t)
(x/define-keys global-map
               '(("C-;" er/expand-region)
                 ("M-q" unfill-region)
                 ("H-q" (lambda ()
                          (interactive)
                          (mark-whole-buffer)
                          (call-interactively #'unfill-region)))))


(x/append-init-hook #'global-page-break-lines-mode)
(setq page-break-lines-max-width 80)
(with-eval-after-load 'page-break-lines
  (when
      (fboundp 'diminish)
    (diminish 'page-break-lines-mode)))

(x/append-init-hook #'global-hl-line-mode)
;; (with-eval-after-load 'hl-line
;;   (set-face-background 'hl-line "#2a2e48"))


(add-hook 'prog-mode-hook #'color-identifiers-mode)

(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(setq highlight-indent-guides-responsive nil)
(setq highlight-indent-guides-delay 0.5)
(setq highlight-indent-guides-auto-odd-face-perc 3)
(setq highlight-indent-guides-auto-even-face-perc 6)

(x/append-init-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)
(add-hook 'org-mode-hook #'rainbow-mode)
(add-hook 'css-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(with-eval-after-load 'rainbow-mode
  (when (fboundp 'diminish)
    (diminish 'rainbow-mode)))

(add-hook 'css-mode-hook #'rainbow-identifiers-mode)
(add-hook 'html-mode-hook #'rainbow-identifiers-mode)
(add-hook 'prog-mode-hook #'rainbow-identifiers-mode)
(with-eval-after-load 'rainbow-identifiers
  (when (fboundp 'diminish)
    (diminish 'rainbow-identifiers-mode)))

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-src-mode-hook #'rainbow-delimiters-mode)

(setq htmlize-pre-style t)

(x/append-init-hook #'show-paren-mode)

(x/append-init-hook #'smartparens-global-mode)
(setq sp-hybrid-kill-entire-symbol nil)
;; https://stackoverflow.com/questions/22107501/set-emacs-to-smart-auto-line-after-a-parentheses-pair
(with-eval-after-load 'smartparens
  (require 'smartparens-config)
  (sp-with-modes '(js-mode json-mode web-mode kotlin-mode css-mode typescript-mode)
    (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "[" nil :post-handlers '(:add ("||\n[i]" "RET")))
    (sp-local-pair "(" nil :post-handlers '(:add ("||\n[i]" "RET")))))

(custom-set-faces
 '(quick-peek-border-face
   ((t
     (:background "#75b79e" :height 0.1))))
 '(quick-peek-padding-face
   ((t
     (:height 0.1)))))

(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'x-editor)
;;; x-editor.el ends here
