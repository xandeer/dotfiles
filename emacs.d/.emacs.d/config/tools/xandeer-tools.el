;;; xandeer-tools.el --- Xandeer's Emacs Configuration editor file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Tools.

;;; Code:

(straight-use-package 'anzu)
(leaf anzu
  :doc "anzu.el is an Emacs port of anzu.vim."
  :url "https://github.com/emacsorphanage/anzu"
  :hook ((after-init-hook . global-anzu-mode))
  :bind (([remap query-replace]        . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(straight-use-package 'avy)

(straight-register-package
 '(pinyinlib :host github
             :repo "xlshiz/pinyinlib.el"))
(straight-use-package 'ace-pinyin)
(leaf avy
  :doc "Jump to things in Emacs tree-style."
  :url "https://github.com/abo-abo/avy"
  :bind
  ("C-:"     . avy-goto-char)
  ("C-'"     . avy-goto-char-2)
  ("C-`"     . avy-goto-char-2)
  ("M-g a"   . beginning-of-buffer)
  ("M-g e"   . end-of-buffer)
  ("M-g M-a" . beginning-of-buffer-other-window)
  ("M-g M-e" . end-of-buffer-other-window)
  ("M-g v"   . scroll-other-window)
  ("M-g M-v" . scroll-other-window-down)
  ("M-g w"   . avy-goto-word-1)
  ("M-g l"   . avy-goto-line)
  ("M-g M-g" . avy-goto-word-1)
  ("C-c C-j" . avy-resume))

(leaf ace-pinyin
  :after avy
  :config
  (ace-pinyin-global-mode 1))

(straight-use-package 'beginend)
(leaf beginend
  :doc "Emacs package to redefine M-< and M-> for some modes"
  :url "https://github.com/DamienCassou/beginend"
  :hook ((after-init-hook . beginend-global-mode)))

(straight-use-package 'cal-china-x)
(leaf cal-china-x
  :require t
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

;  (require 'xandeer-tools-company)

(straight-use-package 'disable-mouse)
(leaf disable-mouse
  :require t
  :pre-setq
  (disable-mouse-wheel-events . nil)
  :config
  (global-disable-mouse-mode))

(xandeer/s-u-p
  (:when (version< emacs-version "27") emojify))
(leaf emojify
  :commands emojify-mode
  :when (version< emacs-version "27")
  :hook ((after-init-hook . global-emojify-mode))
  :custom (emojify-emoji-styles  . '(unicode github))
          (emojify-display-style . 'unicode))

(straight-use-package 'all-the-icons)
(leaf all-the-icons
  :custom (inhibit-compacting-font-caches . t))

;;;###autoload
(defun save-buffer-maybe-show-errors ()
  "Save buffer and show errors if any."
  (interactive)
  (save-buffer)
  (when (not flycheck-current-errors)
    (flycheck-list-errors)))

;;;###autoload
(defun xandeer/flycheck-inline-init ()
  "Flycheck-inline-mode init function"
  (gsetq flycheck-inline-display-function
         (lambda (msg pos)
           (let* ((ov (quick-peek-overlay-ensure-at pos))
                  (contents (quick-peek-overlay-contents ov)))
             (setf (quick-peek-overlay-contents ov)
                   (concat contents (when contents "\n") msg))
             (quick-peek-update ov)))
         flycheck-inline-clear-function #'quick-peek-hide))

(xandeer/s-u-p flycheck)
(leaf flycheck
  ;; :bind (("C-x C-s" . save-buffer-maybe-show-errors))
  :hook prog-mode-hook
  :custom
  (flycheck-display-errors-function
   . #'flycheck-display-error-messages-unless-error-list)
  (flycheck-check-syntax-automatically . '(save idle-change mode-enabled))
  (flycheck-display-errors-delay       . 0.25)
  :bind
  (:flycheck-error-list-mode-map
   ("C-n" . flycheck-error-list-next-error)
   ("C-p" . flycheck-error-list-previous-error)
   ("RET" . flycheck-error-list-goto-error)
   ([return]  . flycheck-error-list-goto-error))
  :config
  (defalias 'show-error-at-point-soon
    'flycheck-show-error-at-point)
  (add-to-list 'flycheck-emacs-lisp-checkdoc-variables 'sentence-end-double-space))

(xandeer/s-u-p flycheck-package)
(leaf flycheck-package
  :doc "Flycheck checker for elisp package metadata."
  :url "https://github.com/purcell/flycheck-package"
  :tag "lisp"
  :after flycheck
  :config
  (flycheck-package-setup))

(xandeer/s-u-p
  (:when *flycheck-inline* flycheck-inline quick-peek))
(leaf flycheck-inline
  :hook flycheck-mode-hook
  :mode-hook (xandeer/flycheck-inline-init))

(xandeer/s-u-p
  (:when *is-a-mac* grab-mac-link))
(leaf grab-mac-link
  :doc "Grab link from Mac Apps."
  :url "https://github.com/xuchunyang/grab-mac-link.el"
  :when *is-a-mac*)

(leaf grep
  :tag "builtin"
  :custom
  ((grep-highlight-matches grep-scroll-output) . t))

(leaf xandeer-tools-ibuffer
  :require t)

(xandeer/s-u-p imenu-list)
(leaf imenu-list
  :doc "Emacs plugin to show the current buffer's imenu entries in a seperate buffer"
  :url "https://github.com/bmag/imenu-list"
  :tag "tools" "convenience"
  :bind (("C-." . imenu-list-smart-toggle))
  :custom (imenu-list-auto-resize . t))

;; (xandeer/s-u-p isearch)
(leaf isearch
  :doc "Isearch, that is, incremental search, is the standard way to search in
vanilla Emacs."
  :url "https://www.emacswiki.org/emacs/IncrementalSearch"
  :tag "matching"
  :bind (:isearch-mode-map
         ([remap isearch-delete-char]
          . isearch-del-char)
         ("C-w"
          . isearch-yank-symbol)
         ([(control return)]
          . isearch-exit-other-end)
         ("C-o"
          . isearch-occur))

  :defer-config
  ;; Search back/forth for the symbol at point
  ;; See http://www.emacswiki.org/emacs/SearchAtPoint
  (defun isearch-yank-symbol ()
    "*Put symbol at current point into search string."
    (interactive)
    (let ((sym (thing-at-point 'symbol)))
      (if sym
          (progn
            (setq isearch-regexp    t
                  isearch-string    (regexp-quote sym)
                  isearch-message   (mapconcat 'isearch-text-char-description isearch-string "")
                  isearch-yank-flag t))
        (ding)))
    (isearch-search-and-update))

  ;; http://www.emacswiki.org/emacs/ZapToISearch
  (defun isearch-exit-other-end (rbeg rend)
    "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
    (interactive "r")
    (isearch-exit)
    (goto-char isearch-other-end)))

(require 'xandeer-tools-ivy)

(straight-use-package 'prescient)
(leaf prescient
  :doc "☄️ Simple but effective sorting and filtering for Emacs."
  :url "https://github.com/raxod502/prescient.el"
  :tag "extensions"
  :hook ((after-init-hook . prescient-persist-mode)))

(xandeer/s-u-p projectile)
(leaf projectile
  :doc "Projectile is a project interaction library for Emacs."
  :url "https://github.com/bbatsov/projectile"
  :tag "project" "convenience"
  :bind ([remap find-tag] . projectile-find-tag)
  :hook after-init-hook
  :custom
  (projectile-indexing-method      . 'hybrid)
  (projectile-require-project-root . 'prompt)
  :config
  (gsetq projectile-project-root-files-top-down-recurring
         (append '("compile_commands.json"
                   ".cquery")
                 projectile-project-root-files-top-down-recurring)))

(straight-use-package 'recentf)
(leaf recentf
  :doc "Recentf is a minor mode that builds a list of recently opened files."
  :url "https://www.emacswiki.org/emacs/RecentFiles"
  :tag "files"
  :hook after-init-hook
  :custom
  (recentf-filename-handlers
   . '(;; Text properties inflate the size of recentf's files, and there is
       ;; no purpose in persisting them, so we strip them out.
       substring-no-properties
       ;; Resolve symlinks of local files. Otherwise we get duplicate
       ;; entries opening symlinks.
       xandeer--recent-file-truename
       ;; Replace $HOME with ~, which is more portable, and reduces how much
       ;; horizontal space the recentf listing uses to list recent files.
       abbreviate-file-name))
  (recentf-max-saved-items . 100)
  (recentf-exclude         . '("/tmp/" "/ssh:"))
  :config
  (defun xandeer--recent-file-truename (file)
    (if (or (file-remote-p file nil t)
            (not (file-remote-p file)))
        (file-truename file)
      file))

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(straight-use-package 'restclient)
(straight-use-package 'ob-restclient)

(when (or *is-a-mac* *is-a-linux*)
  (require 'xandeer-tools-rime))

(straight-register-package
'(sdcv :host github
  :repo "manateelazycat/sdcv"))

(straight-use-package 'sdcv)
(leaf sdcv
  :commands (sdcv-search-pointer
             sdcv-search-pointer+
             sdcv-search-input
             sdcv-search-input+)
  :bind
  ("C-c x l" . sdcv-search-pointer+)
  ("C-c x k" . sdcv-search-input+)
  :config
  (set-face-attribute 'sdcv-tooltip-face nil
                      :foreground "#E0F0E9")
  (setq sdcv-say-word-p nil
        sdcv-tooltip-timeout 10
        sdcv-dictionary-data-dir  (expand-file-name "~/.stardict")))

(when (or *is-a-mac* *is-a-linux*)
  (straight-use-package 'sis)
  (leaf sis
    :disabled t
    ;; :hook
    ;; enable the /follow context/ and /inline region/ mode for specific buffers
    ;; (((text-mode prog-mode) . sis-follow-context-mode)
    ;;  ((text-mode prog-mode) . sis-inline-mode))
    :config
    (sis-ism-lazyman-config
     nil
     "rime"
     'native)
    :custom
    ;; enable the /cursor color/ mode
    ((;;sis-global-cursor-color-mode
      ;; enable the /respect/ mode
      sis-global-respect-mode
      ;; enable the /follow context/ mode for all buffers
      sis-global-follow-context-mode t
      ;; enable the /inline english/ mode for all buffers
      sis-global-inline-mode)
     . t)))

(when (or *is-a-mac* *is-a-linux*)
  (require 'xandeer-tools-telega))

(straight-use-package 'treemacs)
(leaf treemacs
  :doc "A tree layout file explorer for Emacs"
  :url "https://github.com/Alexander-Miller/treemacs"
  :tag "convenience" "files"
  :init
  (after-x 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :defer-config
  (gsetq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
         treemacs-deferred-git-apply-delay      0.5
         treemacs-directory-name-transformer    #'identity
         treemacs-display-in-side-window        t
         treemacs-eldoc-display                 t
         treemacs-file-event-delay              5000
         treemacs-file-extension-regex          treemacs-last-period-regex-value
         treemacs-file-follow-delay             0.2
         treemacs-file-name-transformer         #'identity
         treemacs-follow-after-init             t
         treemacs-git-command-pipe              ""
         treemacs-goto-tag-strategy             'refetch-index
         treemacs-indentation                   2
         treemacs-indentation-string            " "
         treemacs-is-never-other-window         nil
         treemacs-max-git-entries               5000
         treemacs-missing-project-action        'ask
         treemacs-move-forward-on-expand        t
         treemacs-no-png-images                 nil
         treemacs-no-delete-other-windows       t
         treemacs-project-follow-cleanup        nil
         treemacs-persist-file                  (no-littering-expand-var-file-name "treemacs-persist")
         treemacs-position                      'left
         treemacs-recenter-distance             0.1
         treemacs-recenter-after-file-follow    nil
         treemacs-recenter-after-tag-follow     nil
         treemacs-recenter-after-project-jump   'always
         treemacs-recenter-after-project-expand 'on-distance
         treemacs-show-cursor                   nil
         treemacs-show-hidden-files             t
         treemacs-silent-filewatch              nil
         treemacs-silent-refresh                nil
         treemacs-sorting                       'alphabetic-desc
         treemacs-space-between-root-nodes      t
         treemacs-tag-follow-cleanup            t
         treemacs-tag-follow-delay              1.5
         treemacs-user-mode-line-format         nil
         treemacs-user-header-line-format       nil
         treemacs-width                         35)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;; (treemacs-resize-icons 44)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))


(straight-use-package 'treemacs-icons-dired)
(leaf treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(straight-use-package 'treemacs-magit)
(leaf treemacs-magit
  :after treemacs magit)

(straight-use-package 'treemacs-projectile)
(leaf treemacs-projectile
  :after treemacs projectile)

(leaf which-func
  :tag "builtin"
  :hook after-init-hook)

(straight-use-package 'which-key)
(leaf which-key
  :doc "Emacs package that displays available keybindings in popup."
  :url "https://github.com/justbur/emacs-which-key"
  :tag "help"
  :custom (which-key-allow-imprecise-window-fit . nil)
  :hook 'after-init-hook)

(provide 'xandeer-tools)
;;; xandeer-tools.el ends here
