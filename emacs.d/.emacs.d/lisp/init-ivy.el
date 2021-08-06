;;; init-ivy.el --- ivy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf ivy
  :straight t wgrep
  :hook after-init-hook
  :bind
  ("C-r" . ivy-resume)
  (:ivy-minibuffer-map
   ("TAB" . ivy-alt-done))
  :custom
  ((ivy-wrap                         . t)
   (ivy-auto-shrink-minibuffer-alist . '((t . nil)))
   (ivy-height                       . 15)
   (ivy-fixed-height-minibuffer      . t)
   (projectile-completion-system     . 'ivy)
   ;; disable magic slash on non-match
   (ivy-magic-slash-non-match-action . nil)
   ;; don't show recent files in switch-buffer
   (ivy-use-virtual-buffers          . nil)
   ;; ...but if that ever changes, show their full path
   (ivy-virtual-abbreviate           . 'full)
   ;; don't quit minibuffer on delete-error
   (ivy-on-del-error-function        . #'ignore)
   ;; enable ability to select prompt (alternative to `ivy-immediate-done')
   (ivy-use-selectable-prompt        . t))
  :config
  ;; Highlight each ivy candidate including the following newline, so that it
  ;; extends to the right edge of the window
  (setf (alist-get 't ivy-format-functions-alist)
        #'ivy-format-function-line))

(leaf ivy-xref
  :straight t
  :custom (xref-show-xrefs-function . #'ivy-xref-show-xrefs))

(leaf counsel
  :straight t
  :custom
  (counsel-find-file-at-point         . t)
  ;; Don't use ^ as initial input. Set this here because `counsel' defines more
  ;; of its own, on top of the defaults.
  (ivy-initial-inputs-alist           . nil)
  ;; helpful
  (counsel-describe-function-function . #'helpful-callable)
  (counsel-describe-variable-function . #'helpful-variable)
  :bind
  (([remap apropos]                  . counsel-apropos)
   ([remap bookmark-jump]            . counsel-bookmark)
   ([remap describe-bindings]        . counsel-descbinds)
   ([remap describe-function]        . counsel-describe-function)
   ([remap describe-variable]        . counsel-describe-variable)
   ([remap describe-face]            . counsel-faces)
   ([remap execute-extended-command] . counsel-M-x)
   ([remap find-file]                . counsel-find-file)
   ([remap imenu]                    . counsel-imenu)
   ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
   ([remap load-theme]               . counsel-load-theme)
   ([remap locate]                   . counsel-locate)
   ([remap recentf-open-files]       . counsel-recentf)
   ([remap set-variable]             . counsel-set-variable)
   ([remap swiper]                   . counsel-grep-or-swiper)
   ([remap switch-to-buffer]         . counsel-buffer-or-recentf)
   ([remap unicode-chars-list-chars] . counsel-unicode-char)
   ([remap yank-pop]                 . counsel-yank-pop))
  ("C-x C-b" . counsel-switch-buffer-other-window)
  ("C-c b"   . counsel-buffer-or-recentf)
  ("C-c C-b" . xr/counsel-buffer-or-recentf-other-window)
  ("C-s"     . swiper)
  ("C-c C-r" . ivy-resume)
  ("C-c x s" . xr/search-cwd)
  ("C-c x S" . xr/search-other-cwd)
  ("C-c f f" . counsel-projectile-find-file)
  ("C-c f r" . counsel-recentf)
  (:counsel-find-file-map
   ("C-h" . counsel-up-directory)
   ("C-l" . counsel-down-directory))
  :config
  (defun xr/counsel-buffer-or-recentf-other-window ()
    "Switch to recent file in another window."
    (interactive)
    (ivy-read "Switch to buffer or recentf in another window:"
              (counsel-buffer-or-recentf-candidates)
              :action (lambda (s)
                        (if (bufferp s)
                            (switch-to-buffer-other-window s)
                          (find-file-other-window s)))
              :caller 'xr/counsel-buffer-or-recentf-other-window))
  (setq counsel-rg-base-command
        '("rg" "--hidden" "-M" "240" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s"))
  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  ;; (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)
  (ivy-add-actions
   'counsel-rg ; also applies to `counsel-rg'
   '(("O" xr/ivy-git-grep-other-window-action "open in other window")))

  (with-eval-after-load 'savehist
    ;; Persist `counsel-compile' history
    (add-to-list 'savehist-additional-variables 'counsel-compile-history))

  ;; `counsel-imenu' -- no sorting for imenu. Sort it by appearance in page.
  (add-to-list 'ivy-sort-functions-alist '(counsel-imenu))

  ;; `counsel-locate'
  ;; Use spotlight on mac by default since it doesn't need any additional setup
  (setq counsel-locate-cmd #'counsel-locate-cmd-mdfind)

  ;; `counsel-find-file'
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(leaf counsel-projectile
  :straight t
  :bind
  (([remap projectile-find-file]        . counsel-projectile-find-file)
   ([remap projectile-find-dir]         . counsel-projectile-find-dir)
   ([remap projectile-switch-to-buffer] . counsel-projectile-switch-to-buffer)
   ([remap projectile-grep]             . counsel-projectile-grep)
   ([remap projectile-ag]               . counsel-projectile-ag)
   ([remap persp-switch-to-buffer]      . counsel-projectile-find-file)
   ([remap projectile-switch-project]   . counsel-projectile-switch-project))
  :config
  (with-eval-after-load 'prescient
    (setq counsel-projectile-sort-files t)))

;; There's a bug after v5.0, so after clone, check to v5.0.
(leaf ivy-prescient
  :doc "☄️ Simple but effective sorting and filtering for Emacs."
  :url "https://github.com/raxod502/prescient.el"
  :straight t prescient
  :hook ivy-mode-hook
  :mode-hook (prescient-persist-mode 1)
  :custom
  (ivy-prescient-retain-classic-highlighting . t)
  :config
  (defun xr/enable-pinyin ()
    (interactive)
    (setq prescient-filter-method '(literal regexp initialism pinyin)))
  (defun xr/disable-pinyin ()
    (interactive)
    (setq prescient-filter-method '(literal regexp initialism)))
  (xr/disable-pinyin)
  (add-hook 'minibuffer-exit-hook 'xr/disable-pinyin)
  (add-hook 'minibuffer-setup-hook 'xr/enable-pinyin))

(leaf all-the-icons-ivy-rich
  :straight t
  :defvar xr/all-the-icons-ivy-rich-reload-p
  :custom
  (all-the-icons-ivy-rich-icon-size . 0.7)
  :init
  (all-the-icons-ivy-rich-mode 1)
  (setq xr/all-the-icons-ivy-rich-reload-p nil)
  (defun xr/ivy-rich-reload ()
    (if (and all-the-icons-ivy-rich-mode
             xr/all-the-icons-ivy-rich-reload-p)
        (advice-remove #'counsel-M-x #'xr/ivy-rich-reload)
      (all-the-icons-ivy-rich-reload)
      (setq xr/all-the-icons-ivy-rich-reload-p t)))
  (defun xr/all-the-icons-ivy-rich-align-icons ()
    "Set tab size to 1, to insert tabs as delimiters."
    (setq-local tab-width 2))
  :advice
  (:before counsel-M-x xr/ivy-rich-reload)
  (:override all-the-icons-ivy-rich-align-icons xr/all-the-icons-ivy-rich-align-icons))

(leaf pcre2el
  :straight t)
(leaf ivy-rich
  :straight t
  :init (ivy-rich-mode 1))


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
Returns THING if it is a string.  Otherwise, if nothing is found
at point and PROMPT is non-nil, prompt for a string (if PROMPT is
a string it'll be used as the prompting string).  Returns nil if
all else fails.
NOTE: Don't use THING for grabbing `symbol-at-point`.  The xref
fallback is smarter in some cases."
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
(cl-defun xr/ivy-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.
  :query STRING
    Determines the initial input to search for.
  :in PATH
    Sets what directory to base the search out of. Defaults to the current
    project's root.
  :recursive BOOL
    Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'counsel)
  (defun xr/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))
  (let* ((this-command 'counsel-rg)
         (project-root (or (xr/project-root) default-directory))
         (directory (or in project-root))
         (args (concat (if all-files " -uu")
                       (unless recursive " --maxdepth 1")
                       " "
                       (mapconcat #'shell-quote-argument args " "))))
    (setq deactivate-mark t)
    (counsel-rg
     (or query
         (when (xr/region-active-p)
           (replace-regexp-in-string
            "[! |]" (lambda (substr)
                      (cond ((and (string= substr " ")
                                  (not *ivy-fuzzy*))
                             "  ")
                            ((string= substr "|")
                             "\\\\\\\\|")
                            ((concat "\\\\" substr))))
            (rxt-quote-pcre (xr/thing-at-point-or-region)))))
     directory args
     (or prompt
         (format "rg%s [%s]: "
                 args
                 (cond ((equal directory default-directory)
                        "./")
                       ((equal directory project-root)
                        (projectile-project-name))
                       ((file-relative-name directory project-root))))))))

;;;###autoload
(defun xr/ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.
  If ARG (universal argument), include all files, even hidden or compressed ones,
  in the search."
  (interactive "P")
  (xr/ivy-file-search :query initial-query :in directory :all-files arg))

;;;###autoload
(defun xr/ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.
  If ARG (universal argument), include all files, even hidden or compressed ones."
  (interactive "P")
  (xr/ivy/project-search arg initial-query default-directory))

;;;###autoload
(defun xr/search-cwd (&optional arg)
  "Conduct a text search in files under the current folder.
If prefix ARG is set, prompt for a directory to search from."
  (interactive "P")
  (let ((default-directory
          (if arg (counsel-read-directory-name "Search directory: ")
            default-directory)))
    (call-interactively #'xr/ivy/project-search-from-cwd)))

;;;###autoload
(defun xr/search-other-cwd ()
  "Conduct a text search in another directory."
  (interactive)
  (xr/search-cwd 'other))

(provide 'init-ivy)
;;; init-ivy.el ends here
