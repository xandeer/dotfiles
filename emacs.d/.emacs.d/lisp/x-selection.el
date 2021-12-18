;;; x-selection.el --- x-selection -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defgroup x/selection nil
  "Selection navigation and editing with region."
  :group 'bindings
  :prefix "x/selection-")

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

(defun x--get-region-str ()
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))

(defvar x/selection-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (let ((map (copy-keymap x/selection-mode-map-base)))
    ;; navigation
    (define-key map "d" #'exchange-point-and-mark)
    (define-key map "f" #'jieba-forward-word)
    (define-key map "b" #'jieba-backward-word)
    (define-key map "j" #'next-line)
    (define-key map "k" #'previous-line)
    ;; expand
    (define-key map "o" #'er/expand-region)
    (define-key map "i" #'er/contract-region)
    ;; clipboard
    (define-key map "w" #'easy-kill)
    ;; misc
    (define-key map "n" #'x/toggle-narrow)
    ;; search
    (define-key map "s" (lambda () (interactive) (consult-line (x--get-region-str))))
    (define-key map "l" #'sdcv-search-pointer)
    (define-key map "L" #'go-translate)
    (define-key map "G" (lambda () (interactive) (engine/search-google (x--get-region-str))))
    (define-key map "S" (lambda () (interactive) (consult-ripgrep default-directory (x--get-region-str))))
    map))

;;;###autoload
(defun x/selection-forward ()
  (interactive)
  (unless (equal (point) (region-end))
    (exchange-point-and-mark))
  (call-interactively #'jieba-forward-word))

;;;###autoload
(define-minor-mode x/selection-mode
  "Minor mode for navigating and editing with region.

When `x/selection-mode` is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], call commands instead of self-inserting
when the region is active.

\\{x/selection-mode-map}"
  :keymap x/selection-mode-map
  :group 'x/selection
  :lighter " X/S"
  (if x/selection-mode
      (progn
        (lispy-raise-minor-mode 'x/selection-mode))))

(defun x/selection-mode-p ()
  (equal x/selection-mode t))

(defun x/selection-mode-enable ()
  (unless (equal major-mode 'emacs-lisp-mode)
    (x/selection-mode)))

(defun x/selection-mode-disable ()
  (x/selection-mode -1))

(defun x/selection-mode-global-enable ()
  (interactive)
  (add-hook 'activate-mark-hook #'x/selection-mode-enable)
  (add-hook 'deactivate-mark-hook #'x/selection-mode-disable))

(defun x/selection-mode-global-disable ()
  (interactive)
  (remove-hook 'activate-mark-hook #'x/selection-mode-enable)
  (remove-hook 'deactivate-mark-hook #'x/selection-mode-disable))

(provide 'x-selection)
;;; x-selection.el ends here
