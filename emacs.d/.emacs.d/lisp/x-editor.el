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
(add-hook 'nov-mode-hook (lambda () (whitespace-mode -1)))

(autoload 'auto-compression-mode "jka-cmpr" nil t)
(x/append-init-hook #'auto-compression-mode)

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

;;; misc
(defun x/flush-double-newlines ()
  "Replace double newlines with one."
  (interactive)
  (save-excursion
    (replace-regexp "\n\n\n" "\n\n")))

(defun x/duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (progn
    (move-beginning-of-line 1)
    (insert (thing-at-point 'line))
    (move-end-of-line 1)))

(defun x/toggle-narrow (arg)
  (interactive "p")
  (if (buffer-narrowed-p)
      (widen)
    (cond ((region-active-p)
           (narrow-to-region (region-beginning) (region-end)))
          (lispy-mode
           (lispy-narrow arg))
          ((equal major-mode 'org-mode)
           (org-toggle-narrow-to-subtree))
          (smartparens-mode
           (sp-narrow-to-sexp arg)))))

(defun x/new-line-before ()
  "Insert a new line before the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun x/new-line-after ()
  "Insert a new line after the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(provide 'x-editor)
;;; x-editor.el ends here
