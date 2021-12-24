;;; x-point-mode.el --- x-point-mode -*- lexical-binding: t -*-
;;; Commentary:
;; Start from lispy

;;; Code:

;;; base
(defvar x-point-left "[([{]"
  "Opening delimiter.")

(defvar x-point-right "[])}]"
  "Closing delimiter.")

(defvar x-point-outline "^;;\\(?:;[^#]\\|\\*+\\)"
  "Outline delimiter.")

(defsubst x-point-right-p ()
  "Return t if after variable `x-point-right'."
  (looking-back x-point-right
                (line-beginning-position)))

(defsubst x-point-left-p ()
  "Return t if before variable `x-point-left'."
  (looking-at x-point-left))

(defun x-point-bolp ()
  "Return t if point is at beginning of line, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun x-point-looking-back (regexp)
  "Forward to (`looking-back' REGEXP)."
  (looking-back regexp (line-beginning-position)))

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
  "Special point predicates.")

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

(autoload #'lispy-view "lispy" nil t)
(defvar x-point-mode-special-map-base
  (let ((map (make-sparse-keymap)))
    ;; navigation
    (x-point-define-key map "e" #'end-of-line)
    (x-point-define-key map "j" #'next-line)
    (x-point-define-key map "k" #'previous-line)
    (x-point-define-key map "J" #'avy-goto-line-below)
    (x-point-define-key map "K" #'avy-goto-line-above)
    ;; misc
    (x-point-define-key map "n" #'x/toggle-narrow)
    (x-point-define-key map "v" #'lispy-view)
    ;; digit argument
    (mapc (lambda (x) (x-point-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))
    map))

;;;###autoload
(define-minor-mode x-point-mode
  "Minor mode for navigating and editing with region.

When `x-point-mode` is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], call commands instead of self-inserting
at some special points.

\\{x-point-mode-special-map-base}"
  :keymap x-point-mode-special-map-base
  :group 'x-point
  :lighter " x/p"
  (if x-point-mode
      (progn
        (lispy-raise-minor-mode 'x-point-mode))))

;;; org-mode
(defvar x-point-org-block-begin-re "^#\\+\\(begin\\|BEGIN\\)_"
  "Org block begin delimiter.")

(defvar x-point-org-block-end-re "^#\\+\\(end\\|END\\)_.*"
  "Org block begin delimiter.")

(defun x-point-org-block-begin-p ()
  (looking-at x-point-org-block-begin-re))

(defun x-point-org-block-end-p ()
  (or (looking-at x-point-org-block-end-re)
      (x-point-looking-back x-point-org-block-end-re)))

(defun x-point-org-different ()
  "Switch to the different side of currrent context."
  (interactive)
  (cond ((x-point-org-block-end-p)
         (re-search-backward x-point-org-block-begin-re))
        ((x-point-org-block-begin-p)
         (progn
           (re-search-forward  x-point-org-block-end-re)
           (end-of-line)))
        (t (lispy-different))))

(defun x-point-define-org-map (parent)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent)
    ;; navigation
    (x-point-define-key map "a" #'org-beginning-of-line)
    (x-point-define-key map "e" #'org-end-of-line)
    (x-point-define-key map "d" #'x-point-org-different)
    (x-point-define-key map "j" #'next-line)
    (x-point-define-key map "k" #'previous-line)
    ;; (x-point-define-key map "o" #'org-up-element)
    ;; misc
    (x-point-define-key map "v" #'lispy-view)
    map))

(defvar x-point-mode-org-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map
     (x-point-define-org-map x-point-mode-special-map-base))
    (x-point-define-key map "o" #'org-up-element)
    map))

;;;###autoload
(define-minor-mode x-point-org-mode
  "Minor mode for navigating and editing with org mode.

When `x-point-org-mode` is on, most unprefixed keys,
i.e. [a-zA-Z+-./<>], call commands instead of self-inserting
at some special points.

\\{x-point-mode-org-map}"
  :keymap x-point-mode-org-map
  :group 'x-point
  :lighter " x/p"
  (if x-point-org-mode
      (progn
        (push #'x-point-org-block-end-p x-point-special-p-alist)
        (lispy-raise-minor-mode 'x-point-org-mode))))

(defun x-point-mode-p ()
  (equal x-point-mode t))

;; (add-hook 'org-mode-hook #'x-point-org-mode)

;;; insipired by org speed commands
(defcustom x-point-speed-command-hook
  '(x-point-speed-selection-command-activate org-speed-command-activate x-point-speed-command-activate)
  "Hook for activating speed commands at strategic locations.
Hook functions are called in sequence until a valid handler is
found.

Each hook takes a single argument, a user-pressed command key
which is also a `self-insert-command' from the global map.

Within the hook, examine the cursor position and the command key
and return nil or a valid handler as appropriate.  Handler could
be one of an interactive command, a function, or a form.

Set `x-point-use-speed-commands' to non-nil value to enable this
hook.  The default setting is `x-point-speed-command-activate'."
  :group 'x-point
  ;; :version "24.1"
  :type 'hook)

(defcustom x-point-speed-commands
  '(("Navigation")
    ("e" . end-of-line)
    ("j" . next-line)
    ("k" . previous-line)
    ("J" . avy-goto-line-below)
    ("K" . avy-goto-line-above)
    ("Misc")
    ("n" . x/toggle-narrow)
    ("v" . lispy-view))
  "Alist of speed commands.

The car of each entry is a string with a single letter, which
must be assigned to `self-insert-command' in the global map.

The cdr is either a command to be called interactively, a
function to be called, or a form to be evaluated.

An entry that is just a list with a single string will be
interpreted as a descriptive headline that will be added when
listing the speed commands in the Help buffer using the `?' speed
command."
  :group 'x-point
  ;; :package-version '(Org . "9.5")
  :type '(repeat :value ("k" . ignore)
		             (choice :value ("k" . ignore)
			                   (list :tag "Descriptive Headline" (string :tag "Headline"))
			                   (cons :tag "Letter and Command"
			                         (string :tag "Command letter")
			                         (choice
				                        (function)
				                        (sexp))))))

(defun x-point-speed-command-activate (keys)
  "Hook for activating single-letter speed commands.
See `x-point-speed-commands' for configuring them."
  (when (or (x-point-bol-p)
            (and (functionp org-use-speed-commands)
                 (funcall org-use-speed-commands)))
    (cdr (assoc keys
                x-point-speed-commands))))

(defcustom x-point-speed-selection-commands
  '(("Navigation")
    ("a" . beginning-of-line)
    ("d" . exchange-point-and-mark)
    ("f" . jieba-forward-word)
    ("b" . jieba-backward-word)
    ("Expand")
    ("o" . er/expand-region)
    ("i" . er/contract-region)
    ("Search")
    ("r" . anzu-query-replace-regexp)
    ("s" . x--selection-consult-line)
    ("S" . x--selection-consult-line)
    ("l" . sdcv-search-pointer)
    ("L" . go-translate)
    ("G" . x--selection-google)
    ("Misc")
    ("w" . easy-kill)
    ("Deactivate region")
    ("g" . keyboard-quit))
  "Alist of speed commands.

The car of each entry is a string with a single letter, which
must be assigned to `self-insert-command' in the global map.

The cdr is either a command to be called interactively, a
function to be called, or a form to be evaluated.

An entry that is just a list with a single string will be
interpreted as a descriptive headline that will be added when
listing the speed commands in the Help buffer using the `?' speed
command."
  :group 'x-point
  ;; :package-version '(Org . "9.5")
  :type '(repeat :value ("k" . ignore)
		             (choice :value ("k" . ignore)
			                   (list :tag "Descriptive Headline" (string :tag "Headline"))
			                   (cons :tag "Letter and Command"
			                         (string :tag "Command letter")
			                         (choice
				                        (function)
				                        (sexp))))))

(defun x-point-speed-selection-command-activate (keys)
  "Hook for activating single-letter speed commands.
See `x-point-speed-commands' for configuring them."
  (when (region-active-p)
    (cdr (assoc keys
                (append x-point-speed-selection-commands
                        x-point-speed-commands)))))

(defvar x-point-use-speed-commands t)
(defvar x-point-speed-command nil)
(defun x-point-self-insert-command (N)
  "Like `self-insert-command`. Inspired by `org-speed-commands`."
  (interactive "p")
  (cond
   ((and x-point-use-speed-commands
         (let ((kv (this-command-keys-vector)))
           (setq x-point-speed-command
                 (run-hook-with-args-until-success
                  'x-point-speed-command-hook
                  (make-string 1 (aref kv (1- (length kv))))))))
    (cond
     ((commandp x-point-speed-command)
      (setq this-command x-point-speed-command)
      (call-interactively x-point-speed-command))
     ((functionp x-point-speed-command)
      (funcall x-point-speed-command))
     ((and x-point-speed-command (listp x-point-speed-command))
      (eval x-point-speed-command))
     (t (let (x-point-use-speed-commands)
          (call-interactively 'x-point-self-insert-command)))))
   (t
    (cond
     ((equal major-mode 'org-mode)
      (setq org-table-may-need-update t)
      (self-insert-command N)
      (org-fix-tags-on-the-fly)
      (when org-self-insert-cluster-for-undo
        (if (not (eq last-command 'org-self-insert-command))
            (setq org-self-insert-command-undo-counter 1)
          (if (>= org-self-insert-command-undo-counter 20)
              (setq org-self-insert-command-undo-counter 1)
            (and (> org-self-insert-command-undo-counter 0)
                 buffer-undo-list (listp buffer-undo-list)
                 (not (cadr buffer-undo-list)) ; remove nil entry
                 (setcdr buffer-undo-list (cddr buffer-undo-list)))
            (setq org-self-insert-command-undo-counter
                  (1+ org-self-insert-command-undo-counter))))))
     (t
      (self-insert-command N))))))

(defun x-point-remap-org ()
  (define-key org-mode-map [remap self-insert-command] 'x-point-self-insert-command))

(with-eval-after-load 'org-keys
  (x-point-remap-org))

(provide 'x-point-mode)
;;; x-point-mode.el ends here
