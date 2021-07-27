;;; init-basic.el --- Basic -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
  blink-cursor-interval .6
  blink-matching-paren  t
  cursor-in-non-selected-windows t)
(blink-cursor-mode 1)

(setq-default
 show-paren-style                                'parenthesis
 sp-autoinsert-quote-if-followed-by-closing-pair t
 sp-base-key-bindings                            'paredit
 sp-show-pair-from-inside                        t)

(setq hscroll-margin                  3
      scroll-margin                   3
      hscroll-step                    3
      scroll-step                     3
      scroll-conservatively           100000
      scroll-preserve-screen-position 'always
      scroll-error-top-bottom         t)

(setq-default
 fill-column                    76
 visual-fill-column-width       80
 word-wrap                      t
 highlight-indent-guides-method 'column
 tab-width                      2
 tooltip-delay                  1.5)

(setq-default standard-indent 2)

(setq enable-recursive-minibuffers t)

(add-hook #'after-init-hook #'(lambda () (minibuffer-depth-indicate-mode 1)))
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(delete-selection-mode t)

(setq-default
 whitespace-line-column 76
 whitespace-style       '(face spaces tabs newline
                          space-mark tab-mark newline-mark
                          lines-tail empty))

(setq-default
   bookmark-default-file (no-littering-expand-var-file-name "bookmarks.el")
   case-fold-search      t
   ediff-split-window-function 'split-window-horizontally
   ediff-window-setup-function 'ediff-setup-windows-plain
   indent-tabs-mode      nil
   make-backup-files     nil
   mouse-yank-at-point   t
   require-final-newline nil
   save-interprogram-paste-before-kill t
   set-mark-command-repeat-pop    t
   tab-always-indent              'complete
   truncate-lines                 nil
   truncate-partial-width-windows nil)

(leaf exec-path-from-shell
  :straight t
  :init
  (setq-default shell-file-name "bash")
  (setenv "PATH" (concat "/usr/local/bin:/run/current-system/sw/bin:" (getenv "PATH")))
  (setenv "JAVA_HOME" "/Applications/Android Studio.app/Contents/jre/jdk/Contents/Home")

  (defun call-process-to-string (program &rest args)
    (with-temp-buffer
      (apply 'call-process program nil (current-buffer) nil args)
      (buffer-string)))

  (defun get-call-process-args-from-shell-command (command)
    (cl-destructuring-bind
        (the-command . args) (split-string command " ")
      (let ((binary-path (executable-find the-command)))
        (when binary-path
          (cons binary-path args)))))

  (defun shell-command-to-string (command)
    (let ((call-process-args
           (get-call-process-args-from-shell-command command)))
      (if call-process-args
          (apply 'call-process-to-string call-process-args)
        (shell-command-to-string command))))

  (defun try-call-process (command)
    (let ((call-process-args
           (get-call-process-args-from-shell-command command)))
      (if call-process-args
          (apply 'call-process-to-string call-process-args))))

  (advice-add 'shell-command-to-string :before-until 'try-call-process)

  (defun call-with-quick-shell-command (fn &rest args)
    (noflet ((shell-command-to-string
              (&rest args)
              (or (apply 'try-call-process args) (apply this-fn args))))
            (apply fn args)))

  (advice-add 'projectile-find-file :around 'call-with-quick-shell-command)
  :custom
  (shell-command-switch . "-c")
  (shell-file-name      . "bash")
  ((exec-path-from-shell-arguments
    exec-path-from-shell-check-startup-files) . nil)
  :config
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path (expand-file-name "~/bin"))
  (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin")))

(leaf all-the-icons
  :straight t
  :custom (inhibit-compacting-font-caches . t))

(leaf which-func
  :tag "builtin"
  :hook after-init-hook)

(leaf which-key
  :straight t
  :custom (which-key-allow-imprecise-window-fit . nil)
  :hook after-init-hook)

(leaf keyfreq
  :straight t
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          forward-char
          backward-char
          previous-line
          next-line
          newline-and-indent))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;;###autoload
(defun xr/insert-current-filename ()
  "Insert current buffer filename."
  (interactive)
  (insert (file-relative-name buffer-file-name)))

;;;###autoload
(defun xr/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (unless (bound-and-true-p lsp-mode)
    (user-error "Not in an LSP buffer"))
  (call-interactively
   (if (xr/region-active-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

;;;###autoload
(defun xr/project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (xr/project-root dir)
       t))

;;;###autoload
(defun xr/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

;;;###autoload
(defun xr/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let (kill-ring)
    (backward-kill-word arg)))

;;;###autoload
(defun xr/region-active-p ()
  "Return non-nil if selection is active."
  (declare (side-effect-free t))
  (use-region-p))

;;;###autoload
(defun xr/region-beginning ()
  "Return beginning position of selection."
  (declare (side-effect-free t))
  (region-beginning))

;;;###autoload
(defun xr/region-end ()
  "Return end position of selection."
  (declare (side-effect-free t))
  (region-end))

;;;###autoload
(defun xr/thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.
Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.
NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((xr/region-active-p)
         (buffer-substring-no-properties
          (xr/region-beginning)
          (xr/region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; A little smarter than using `symbol-at-point', though in most cases,
         ;; xref ends up using `symbol-at-point' anyway.
         (xref-backend-identifier-at-point (xref-find-backend)))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

;;;###autoload
(defalias 'default/newline #'newline)

;;;###autoload
(defun default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (beginning-of-line)
  (save-excursion (newline))
  (indent-according-to-mode))

;;;###autoload
(defun default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;;###autoload
(defun default/yank-pop ()
  "Interactively select what text to insert from the kill ring."
  (interactive)
  (call-interactively
   (cond ((fboundp 'counsel-yank-pop)    #'counsel-yank-pop)
         ((fboundp 'helm-show-kill-ring) #'helm-show-kill-ring)
         ((error "No kill-ring search backend available. Enable ivy or helm!")))))

;;;###autoload
(defun default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun default/insert-file-path (arg)
  "Insert the file name (absolute path if prefix ARG).
If `buffer-file-name' isn't set, uses `default-directory'."
  (interactive "P")
  (let ((path (or buffer-file-name default-directory)))
    (insert
     (if arg
         (abbreviate-file-name path)
       (file-name-nondirectory path)))))

;;;###autoload
(defun default/newline-indent-and-continue-comments-a ()
  "A replacement for `newline-and-indent'.
Continues comments if executed from a commented line, with special support for
languages with weak native comment continuation support (like C-family
languages)."
  (interactive)
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function nil)
    (delete-horizontal-space t)
    (newline nil t)
    (indent-according-to-mode)))

(provide 'init-basic)
;;; init-basic.el ends here
