;;; x-selection.el --- x-selection -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup x/selection nil
  "Selection navigation and editing with region."
  :group 'bindings
  :prefix "x/selection-")

;;;###autoload
(define-minor-mode x/selection-mode
  "Minor mode for navigating and editing with region.

When `x/selection-mode` is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], call commands instead of self-inserting
when the region is active.

\\{x/selection-mode-map}"
  :keymap x/selection-mode-map
  :group 'x/selection
  :lighter " X/S")

;; (add-hook 'activate-mark-hook (lambda () (x/selection-mode)))

(defvar x/selection-mode-map-base
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (define-key map (kbd "C-a") 'lispy-move-beginning-of-line)
    (define-key map (kbd "C-e") 'lispy-move-end-of-line)
    (define-key map (kbd "M-o") 'lispy-left-maybe)
    ;; killing
    (define-key map (kbd "C-k") 'lispy-kill)
    (define-key map (kbd "M-d") 'lispy-kill-word)
    (define-key map (kbd "M-DEL") 'lispy-backward-kill-word)
    ;; misc
    (define-key map (kbd "(") 'lispy-parens)
    (define-key map (kbd ";") 'lispy-comment)
    (define-key map (kbd "M-q") 'lispy-fill)
    (define-key map (kbd "C-j") 'lispy-newline-and-indent)
    (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    ;; tags
    (define-key map (kbd "M-.") 'lispy-goto-symbol)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    map))

(defvar x/selection-mode-map
  (let ((map (copy-keymap x/selection-mode-map-base)))
    ;; navigation
    (define-key map "d" #'exchange-point-and-mark)
    map))

(provide 'x-selection)
;;; x-selection.el ends here
