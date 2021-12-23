;;; x-selection.el --- x-selection -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'x-point-mode)

(defgroup x/selection nil
  "Selection navigation and editing with region."
  :group 'bindings
  :prefix "x/selection-")

(defun x--get-region-str ()
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))

(defun x--selection-consult-line ()
  (interactive)
  (consult-line (x--get-region-str)))

(defun x--selection-consult-rg-default ()
  (interactive)
  (consult-ripgrep default-directory (x--get-region-str)))

(defun x--selection-google ()
  (interactive)
  (engine/search-google (x--get-region-str)))

(defun x--selection-beginning-of-line ()
  (interactive)
  (cond ((equal major-mode 'org-mode) (org-beginning-of-line))
        (t (x/smart-beginning-of-line))))

(defun x--selection-end-of-line ()
  (interactive)
  (cond ((equal major-mode 'org-mode) (org-end-of-line))
        (t (end-of-line))))

(autoload #'er/contract-region "expand-region-core" nil t)
(defvar x/selection-mode-map-base
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map x-point-mode-special-map-base)
    ;; navigation
    (x-point-define-key map "a" #'beginning-of-line)
    ;; (x-point-define-key map "e" #'end-of-line)
    (x-point-define-key map "d" #'exchange-point-and-mark)
    (x-point-define-key map "f" #'jieba-forward-word)
    (x-point-define-key map "b" #'jieba-backward-word)
    ;; expand
    (x-point-define-key map "o" #'er/expand-region)
    (x-point-define-key map "i" #'er/contract-region)
    ;; clipboard
    (x-point-define-key map "w" #'easy-kill)
    ;; search
    (x-point-define-key map "r" #'anzu-query-replace-regexp)
    (x-point-define-key map "s" #'x--selection-consult-line)
    (x-point-define-key map "l" #'sdcv-search-pointer)
    (x-point-define-key map "L" #'go-translate)
    (x-point-define-key map "G" #'x--selection-google)
    (x-point-define-key map "S" #'x--selection-consult-rg-default)
    ;; deactivate region
    (x-point-define-key map "g" #'keyboard-quit)
    map))

(defvar x/selection-mode-map-org
  (x-point-define-org-map x/selection-mode-map-base))

(defun x/selection-mode-update-key-theme ()
  (let ((map
         (cond ((equal major-mode 'org-mode)
                x/selection-mode-map-org)
               (t x/selection-mode-map-base))))
    (setq x/selection-mode-map map)
    (setcdr (assq 'x/selection-mode minor-mode-map-alist)
            map)))

(defvar x/selection-mode-map x/selection-mode-map-base)
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
        (x/selection-mode-update-key-theme)
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
