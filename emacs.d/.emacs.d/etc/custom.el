(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-rich-icon-size 0.7)
 '(counsel-describe-function-function 'helpful-callable)
 '(counsel-describe-variable-function 'helpful-variable)
 '(counsel-find-file-at-point t)
 '(custom-raised-buttons t)
 '(custom-safe-themes
   '("99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" default))
 '(default-input-method "rime")
 '(dired-filter-group-saved-groups
   '(("default"
      ("Git"
       (directory . ".git")
       (file . ".gitignore"))
      ("Directory"
       (directory))
      ("PDF"
       (extension . "pdf"))
      ("LaTeX"
       (extension "tex" "bib"))
      ("Source"
       (extension "c" "cpp" "hs" "rb" "py" "r" "cs" "el" "lisp" "html" "js" "css"))
      ("Doc"
       (extension "md" "rst" "txt"))
      ("Org"
       (extension . "org"))
      ("Archives"
       (extension "zip" "rar" "gz" "bz2" "tar"))
      ("Images"
       (extension "jpg" "JPG" "webp" "png" "PNG" "jpeg" "JPEG" "bmp" "BMP" "TIFF" "tiff" "gif" "GIF")))))
 '(dired-filter-revert 'never)
 '(dired-recursive-deletes 'top)
 '(doom-dracula-brighter-comments t)
 '(doom-dracula-colorful-headers t)
 '(doom-dracula-comment-bg t)
 '(doom-modeline-bar-width 3)
 '(doom-modeline-buffer-encoding nil)
 '(doom-modeline-buffer-file-name-style 'truncate-with-project)
 '(doom-modeline-buffer-modification-icon t)
 '(doom-modeline-buffer-state-icon t)
 '(doom-modeline-checker-simple-format nil)
 '(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode text-mode))
 '(doom-modeline-display-default-persp-name nil)
 '(doom-modeline-enable-word-count t)
 '(doom-modeline-github t)
 '(doom-modeline-height 25)
 '(doom-modeline-icon t)
 '(doom-modeline-indent-info nil)
 '(doom-modeline-lsp t)
 '(doom-modeline-major-mode-color-icon t)
 '(doom-modeline-major-mode-icon t)
 '(doom-modeline-minor-modes t)
 '(doom-modeline-modal-icon t)
 '(doom-modeline-number-limit 99)
 '(doom-modeline-persp-name nil)
 '(doom-modeline-project-detection 'project)
 '(doom-modeline-unicode-fallback t)
 '(doom-modeline-vcs-max-length 12)
 '(doom-modeline-window-width-limit 76)
 '(exec-path-from-shell-arguments nil)
 '(exec-path-from-shell-check-startup-files nil)
 '(find-file-visit-truename t)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-display-errors-delay 0.25)
 '(flycheck-display-errors-function 'flycheck-display-error-messages-unless-error-list)
 '(gcmh-high-cons-threshold 135266304)
 '(gcmh-idle-delay 3600)
 '(gcmh-lows-cons-threshold 8388608 t)
 '(gcmh-verbose t)
 '(git-gutter:added-sign "✚" t)
 '(git-gutter:deleted-sign "✘" t)
 '(git-gutter:disabled-modes '(asm-mode image-mode) t)
 '(git-gutter:modified-sign "❚" t)
 '(git-gutter:visual-line t t)
 '(git-messenger:show-detail t t)
 '(grep-highlight-matches t)
 '(grep-scroll-output t)
 '(htmlize-pre-style t t)
 '(ibuffer-default-sorting-mode 'filename/process)
 '(ibuffer-saved-filter-groups
   '(("Normal"
      ("Dired"
       (mode . dired-mode))
      ("Emacs"
       (or
        (name . "^\\*dashboard\\*$")
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*Backtrace\\*$")))
      ("Term"
       (mode . vterm-mode))
      ("Text"
       (or
        (mode . org-mode)
        (mode . markdown)
        (mode . rst-mode)
        (mode . text-mode)))
      ("TeX"
       (mode . tex-mode))
      ("Languages"
       (or
        (mode . emacs-lisp-mode)
        (mode . haskell-mode)
        (mode . javascript-mode)
        (mode . lisp-mode)
        (mode . python-mode)
        (mode . ruby-mode)
        (mode . rust-mode)
        (mode . html-mode)
        (mode . css-mode)
        (mode . prog-mode)))
      ("GNUs"
       (or
        (mode . message-mode)
        (mode . bbdb-mode)
        (mode . mail-mode)
        (mode . gnus-group-mode)
        (mode . gnus-summary-mode)
        (mode . gnus-article-mode)
        (name . "^\\.bbdb$")
        (name . "^\\.newsrc-dribble")))
      ("Magit"
       (name . "^magit"))
      ("Help"
       (or
        (name . "^\\*Help\\*$")
        (name . "^\\*Apropos\\*$")
        (name . "^\\*info\\*$")
        (name . "^\\*helpful")))
      ("Custom"
       (or
        (mode . custom-mode)
        (name . "^\\*Customize")))
      ("Helm"
       (mode . helm-major-mode)))))
 '(ibuffer-show-empty-filter-groups nil)
 '(imenu-list-auto-resize t t)
 '(inhibit-compacting-font-caches t t)
 '(insert-directory-program "/bin/ls" t)
 '(ivy-auto-shrink-minibuffer-alist '((t)))
 '(ivy-fixed-height-minibuffer t)
 '(ivy-height 15)
 '(ivy-initial-inputs-alist nil)
 '(ivy-magic-slash-non-match-action nil)
 '(ivy-on-del-error-function 'ignore)
 '(ivy-prescient-retain-classic-highlighting t)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers nil)
 '(ivy-virtual-abbreviate 'full)
 '(ivy-wrap t)
 '(mac-command-modifier 'hyper)
 '(mac-function-modifier 'super)
 '(mac-option-modifier 'meta)
 '(mac-right-command-modifier 'super)
 '(magit-diff-refine-hunk t)
 '(magit-todos-exclude-globs '("*.map") t)
 '(minions-mode-line-lighter "✬")
 '(mmm-global-mode 'buffers-with-submode-classes t)
 '(mmm-submode-decoration-level 2 t)
 '(org-allow-promoting-top-level-subtree t)
 '(org-confirm-babel-evaluate nil)
 '(org-cycle-level-faces t)
 '(org-fontify-done-headline t)
 '(org-fontify-emphasized-text t)
 '(org-fontify-todo-headline t)
 '(org-fontify-whole-block-delimiter-line t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-emphasis-markers t)
 '(org-startup-folded 'content)
 '(point-history-ignore-buffer "^ \\*Minibuf\\|^ \\*point-history-show*")
 '(projectile-completion-system 'ivy)
 '(projectile-indexing-method 'hybrid)
 '(projectile-require-project-root 'prompt)
 '(session-use-package t nil (session))
 '(shell-command-switch "-ic" t)
 '(shell-file-name "zsh")
 '(sp-hybrid-kill-entire-symbol nil)
 '(switch-window-shortcut-style 'alphabet)
 '(switch-window-timeout nil)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator " • ")
 '(vc-handled-backends nil)
 '(which-key-allow-imprecise-window-fit nil)
 '(xref-show-xrefs-function 'ivy-xref-show-xrefs))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(quick-peek-border-face ((t (:background "#75b79e" :height 0.1))) nil "Customized with leaf in quick-peek block")
 '(quick-peek-padding-face ((t (:height 0.1))) nil "Customized with leaf in quick-peek block"))
