;;; x-point-mode.el --- x-point-mode -*- lexical-binding: t -*-
;;; Commentary:
;; Start from lispy

;;; Code:

(defvar x-point-left "[([{]"
  "Opening delimiter.")

(defvar x-point-right "[])}]"
  "Closing delimiter.")

(defvar x-point-org-block-begin-re "^#\\+\\(begin\\|BEGIN\\)_"
  "Org block begin delimiter.")

(defvar x-point-org-block-end-re "^#\\+\\(end\\|END\\)_"
  "Org block begin delimiter.")

(defvar x-point-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defsubst x-point-right-p ()
  "Return t if after variable `x-point-right'."
  (looking-back x-point-right
                (line-beginning-position)))

(defsubst x-point-left-p ()
  "Return t if before variable `x-point-left'."
  (looking-at x-point-left))

(defsubst x-point-org-block-begin-p ()
  (looking-at x-point-org-block-begin-re))

(defsubst x-point-org-block-end-p ()
  (looking-at x-point-org-block-end-re))

(defun x-point-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun x-point--in-string-or-comment-p ()
  "Test if point is inside a string or a comment."
  (let* ((sp (syntax-ppss))
         (beg (nth 8 sp)))
    (when (or (eq (char-after beg) ?\")
              (nth 4 sp))
      beg)))

(defun x-point--ensure-visible ()
  "Remove overlays hiding point."
  (let ((overlays (overlays-at (point)))
        ov expose)
    (while (setq ov (pop overlays))
      (if (and (invisible-p (overlay-get ov 'invisible))
               (setq expose (overlay-get ov 'isearch-open-invisible)))
          (funcall expose ov)))))

(defun x-point-bol-p ()
  "Return t if point is at beginning of a line not empty, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (and (bolp)
         (not (eolp)))))

(defvar-local x-point-special-p-alist '(x-point-bol-p)
  "Special point  predicates.")

(defun x-point--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'.
PLIST currently accepts:
- :disable with a mode to disable
- :override with a lambda to conditionally abort command"
  (let ((disable (plist-get plist :disable))
        (override (plist-get plist :override))
        (inserter (plist-get plist :inserter)))
    `(lambda ()
       ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
                (symbol-name def) (documentation def))
       (interactive)
       ,@(when disable `((,disable -1)))
       (unless (looking-at x-point-outline)
         (x-point--ensure-visible))
       (cond ,@(cond ((null override) nil)
                     ((functionp override)
                      `((funcall ,override)))
                     ((eq (car override) 'cond)
                      (cdr override))
                     (t
                      (error "Unexpected :override %S" override)))

             ;; ,@(when (memq 'magit-blame-mode lispy-compat)
             ;;     '(((and (bound-and-true-p magit-blame-mode)
             ;;             (setq lispy--compat-cmd (lookup-key magit-blame-mode-map (this-command-keys))))
             ;;        (call-interactively lispy--compat-cmd))))

             ((region-active-p)
              (call-interactively ',def))

             ((x-point--in-string-or-comment-p)
              (setq this-command 'self-insert-command)
              (call-interactively 'self-insert-command))

             ;; ((or (x-point-left-p)
             ;;      (x-point-right-p)
             ;;      ;; (org-at-block-p)
             ;;      (x-point-org-block-begin-p)
             ;;      (x-point-org-block-end-p)
             ;;      (and (x-point-bolp)
             ;;           (looking-at x-point-outline)))
             ;;  (call-interactively ',def))

             ((seq-find 'funcall x-point-special-p-alist)
              (call-interactively ',def))
             (t
              (setq this-command 'self-insert-command)
              (call-interactively
               (quote
                ,(or inserter
                     'self-insert-command))))))))

(defun x-point-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC).
FUNC is obtained from (`x-point--insert-or-call' DEF PLIST)."
  (declare (indent 3))
  (require 'eldoc)
  (let ((func (defalias (intern (concat "x-point/special-" (symbol-name def)))
                (x-point--insert-or-call def plist))))
    ;; (add-to-list 'ac-trigger-commands func)
    ;; (unless (memq func mc/cmds-to-run-once)
    ;;   (add-to-list 'mc/cmds-to-run-for-all func))
    (eldoc-add-command func)
    (define-key keymap (kbd key) func)))

(defun x-point-block ()
  (org-next-block arg t block-regexp))

(defun x-point-different ()
  "Switch to the different side of currrent context."
  (interactive)
  (cond ((x-point-org-block-end-p)
         (re-search-backward x-point-org-block-begin-re))
        ((x-point-org-block-begin-p)
         (progn
           (re-search-forward  x-point-org-block-end-re)
           (beginning-of-line)))
        (t (lispy-different))))

(defvar x-point-mode-special-map
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (x-point-define-key map "d" #'x-point-different)
    ;; digit argument
    (mapc (lambda (x) (define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    map))

;;;###autoload
(define-minor-mode x-point-mode
  "Minor mode for navigating and editing with region.

When `x-point-mode` is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], call commands instead of self-inserting
at some special points.

\\{x-point-mode-special-map}"
  :keymap x-point-mode-special-map
  :group 'x-point
  :lighter " x/p"
  (if x-point-mode
      (progn
        (lispy-raise-minor-mode 'x-point-mode))))

(provide 'x-point-mode)
;;; x-point-mode.el ends here