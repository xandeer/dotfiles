;;; x-point-mode.el --- x-point-mode -*- lexical-binding: t -*-
;;; Commentary:
;; Start from org speed commands and lispy

;;; Code:

;;; base helper
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

;;; inspired by org speed commands
(defvar x-point-use-speed-commands t)
(defvar x-point-speed-command nil)

(defcustom x-point-speed-command-hook
  '( x-point-speed-command-activate)
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

(defun x-point-speed-activate-p ()
  (and x-point-use-speed-commands
       (or (x-point-bol-p)
           (region-active-p))))

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
     (t (self-insert-command N))))))

;;; base commands
(defcustom x-point-speed-commands
  '(("Navigation")
    ("e" . end-of-line)
    ("j" . (lambda (N) (interactive "p") (next-line N) (beginning-of-line)))
    ("k" . (lambda (N) (interactive "p") (previous-line N) (beginning-of-line)))
    ("J" . avy-goto-line-below)
    ("K" . avy-goto-line-above)
    ("Misc")
    ("n" . x/toggle-narrow)
    ("v" . x-point-view)
    ("h" . x-hydra-hideshow/body)
    ("Digit arguments")
    ("0" . digit-argument)
    ("1" . digit-argument)
    ("2" . digit-argument)
    ("3" . digit-argument)
    ("4" . digit-argument)
    ("5" . digit-argument)
    ("6" . digit-argument)
    ("7" . digit-argument)
    ("8" . digit-argument)
    ("9" . digit-argument))
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

(defun x-point-bol-p ()
  "Return t if point is at beginning of a line not empty, after optional spaces."
  (save-excursion
    (skip-chars-backward " \t")
    (and (bolp)
         (not (eolp)))))

(defun x-point-speed-command-activate (keys)
  "Hook for activating single-letter speed commands.
See `x-point-speed-commands' for configuring them."
  (when (x-point-bol-p)
    (cdr (assoc keys
                x-point-speed-commands))))

(defun x-point-view ()
  "If region actived, recenter by `region-beginning`,
else recenter by the current point."
  (interactive)
  (when (region-active-p)
    (unless (equal (point) (region-beginning))
      (exchange-point-and-mark)))
  (let ((window-line (count-lines (window-start) (point))))
    (if (or (= window-line scroll-margin)
            (and (not (bolp)) (= window-line (1+ scroll-margin))))
        (recenter (or (get 'x-point-recenter :line) 0))
      (put 'x-point-recenter :line window-line)
      (recenter 0))))

;;; selection
(defcustom x-point-speed-selection-commands
  '(("Navigation")
    ("a" . beginning-of-line)
    ("j" . next-line)
    ("k" . previous-line)
    ("d" . exchange-point-and-mark)
    ("f" . jieba-forward-word)
    ("b" . jieba-backward-word)
    ("Expand")
    ("o" . er/expand-region)
    ("i" . er/contract-region)
    ("Search")
    ("r" . anzu-query-replace-regexp)
    ("s" . x-point--selection-consult-line)
    ("S" . x-point--selection-consult-line)
    ("l" . sdcv-search-pointer)
    ("L" . go-translate)
    ("G" . x-point--selection-google)
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

(add-hook 'x-point-speed-command-hook #'x-point-speed-selection-command-activate -50)

(defun x-point--get-region-str ()
  (buffer-substring-no-properties (region-beginning)
                                  (region-end)))

(defun x-point--selection-consult-line ()
  (interactive)
  (consult-line (x-point--get-region-str)))

(defun x-point--selection-consult-rg-default ()
  (interactive)
  (consult-ripgrep default-directory (x-point--get-region-str)))

(defun x-point--selection-google ()
  (interactive)
  (engine/search-google (x-point--get-region-str)))

(defun x-point--selection-beginning-of-line ()
  (interactive)
  (cond ((equal major-mode 'org-mode) (org-beginning-of-line))
        (t (x/smart-beginning-of-line))))

(defun x-point--selection-end-of-line ()
  (interactive)
  (cond ((equal major-mode 'org-mode) (org-end-of-line))
        (t (end-of-line))))

;;; org-mode
(setq x-point-org-speed-commands
      '(("Outline Navigation")
        ("j" . (org-speed-move-safe 'org-next-visible-heading))
        ("k" . (org-speed-move-safe 'org-previous-visible-heading))
        ("f" . (org-speed-move-safe 'org-forward-heading-same-level))
        ("b" . (org-speed-move-safe 'org-backward-heading-same-level))
        ("u" . (org-speed-move-safe 'outline-up-heading))
        ("d" . x-point-org-different)

        ("Outline Visibility")
        (" " . org-display-outline-path)
        ;; ("n" . org-toggle-narrow-to-subtree)
        ("v" . x-point-view)
        ("=" . org-columns)

        ("Outline Structure Editing")
        ("c" . org-cut-subtree)
        ("w" . org-metaup)
        ("s" . org-metadown)
        ("." . org-metaright)
        ("," . org-metaleft)
        (">" . org-shiftmetaright)
        ("<" . org-shiftmetaleft)
        ("^" . org-sort)
        ("R" . org-refile)
        ("m" . org-mark-subtree)
        ("a" . org-insert-heading-after-current)

        ("Clock Commands")
        ("i" . org-clock-in)
        ("l" . org-clock-out)
        ("S" . x/org-schedule)

        ("Meta Data Editing")
        ("t" . org-todo)
        ("z" . org-add-note)
        (";" . org-set-tags-command)
        ("e" . org-set-effort)
        ("E" . org-inc-effort)

        ("Misc")
        ("q" . x/fill-subtree)
        ("o" . org-open-at-point)
        ("?" . org-speed-command-help)))

(defun x-point-org-speed-command-activate (keys)
  "Hook for activating single-letter speed commands.
See `x-point-org-speed-commands' for configuring them."
  (when (and (equal major-mode 'org-mode)
             (bolp)
             (looking-at org-outline-regexp))
    (cdr (assoc keys
                x-point-org-speed-commands))))

(add-hook 'x-point-speed-command-hook #'x-point-org-speed-command-activate -30)

;; override selection mode
(setq x-point-org-special-commands
      '(("d" . x-point-org-different)))

(defun x-point-org-special-command-activate (keys)
  "Hook for activating single-letter speed commands.
See `x-point-org-speed-commands' for configuring them."
  (when (and (equal major-mode 'org-mode)
             (or (x-point-org-block-begin-p)
                 (x-point-org-block-end-p)))
    (cdr (assoc keys
                x-point-org-special-commands))))

(add-hook 'x-point-speed-command-hook #'x-point-org-special-command-activate -90)

(defun x-point-remap-global ()
  (global-set-key [remap self-insert-command] 'x-point-self-insert-command))

(x-point-remap-global)

(defun x-point-remap-org ()
  (define-key org-mode-map [remap self-insert-command] 'x-point-self-insert-command))

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
           (re-search-forward x-point-org-block-end-re)
           (end-of-line)))
        (t (lispy-different))))

(with-eval-after-load 'org-keys
  (x-point-remap-org))

(provide 'x-point-mode)
;;; x-point-mode.el ends here
