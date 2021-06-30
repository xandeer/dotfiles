;;; init-ivy.el --- ivy -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(leaf ivy
  :straight t wgrep
  :hook after-init-hook
  :bind
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
  (([remap apropos]                    . counsel-apropos)
   ([remap bookmark-jump]              . counsel-bookmark)
   ([remap describe-bindings]          . counsel-descbinds)
   ([remap describe-face]              . counsel-faces)
   ([remap describe-function]          . counsel-describe-function)
   ([remap describe-variable]          . counsel-describe-variable)
   ([remap execute-extended-command]   . counsel-M-x)
   ([remap find-file]                  . counsel-find-file)
   ([remap find-library]               . counsel-find-library)
   ([remap imenu]                      . counsel-imenu)
   ([remap info-lookup-symbol]         . counsel-info-lookup-symbol)
   ([remap load-theme]                 . counsel-load-theme)
   ([remap locate]                     . counsel-locate)
   ([remap recentf-open-files]         . counsel-recentf)
   ([remap set-variable]               . counsel-set-variable)
   ([remap swiper]                     . counsel-grep-or-swiper)
   ([remap unicode-chars-list-chars]   . counsel-unicode-char)
   ([remap yank-pop]                   . counsel-yank-pop))
  ("C-c x s" . xr/search-cwd)
  ("C-c x S" . xr/search-other-cwd)
  ("C-c f f" . counsel-projectile-find-file)
  ("C-c f r" . counsel-recentf)
  (:counsel-find-file-map
   ("C-h"  . counsel-up-directory)
   ("C-l" . counsel-down-directory))
  :config
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
    (gsetq counsel-projectile-sort-files t)))

;; There's a bug after v5.0, so after clone, check to v5.0.
(leaf ivy-prescient
  :doc "☄️ Simple but effective sorting and filtering for Emacs."
  :url "https://github.com/raxod502/prescient.el"
  :straight t prescient
  :hook ivy-mode-hook
  :mode-hook (prescient-persist-mode 1)
  :bind
  ("C-x b" . xr/switch-buffer)
  :custom
  (ivy-prescient-retain-classic-highlighting . t)
  :config
  (defun xr/enable-pinyin ()
    (interactive)
    (setq prescient-filter-method '(literal regexp initialism pinyin)))
  (defun xr/disable-pinyin ()
    (interactive)
    (setq prescient-filter-method '(literal regexp initialism)))
  (defun xr/switch-buffer ()
    "Switch to another buffer."
    (interactive)
    (xr/enable-pinyin)
    (call-interactively 'ivy-switch-buffer))
  (xr/disable-pinyin)
  (add-hook 'minibuffer-exit-hook 'xr/disable-pinyin)
  :advice
  (:before org-roam-insert xr/enable-pinyin)
  (:before swiper-isearch xr/enable-pinyin))

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

(leaf ivy-rich
  :straight t
  :init (ivy-rich-mode 1))
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
  (xr/enable-pinyin)
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
