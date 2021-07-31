;;; init-editor.el --- init-editor -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'mmm-mode)

;; Delete multiple consecutive blank characters at once
(leaf hungry-delete
  :straight t
  :hook
  (activate-mark-hook . (lambda () (hungry-delete-mode -1)))
  (deactivate-mark-hook . hungry-delete-mode)
  (after-init-hook . global-hungry-delete-mode))

(leaf ansi-color
  :straight t
  :commands colourise-compilation-buffer
  :hook (compilation-filter-hook . colourise-compilation-buffer)
  :config
  (eval-and-compile
    (defun colourise-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-cOLOR-APPLY-on-region compilation-filter-start
                                    (point-max))))))

(leaf jka-cmpr
  :hook (after-init-hook . auto-compression-mode))

(straight-register-package
 '(auto-save :host github
             :repo "manateelazycat/auto-save"))
(leaf auto-save
  :straight t
  :require t
  :custom
  (auto-save-idle                       . 5)
  (auto-save-silent                     . t)
  (auto-save-delete-trailing-whitespace . t)
  :hook
  (org-capture-mode-hook             . auto-save-disable)
  (org-capture-prepare-finalize-hook . auto-save-enable)
  (after-init-hook                   . auto-save-enable))

(leaf easy-kill
  :straight t
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp]      . easy-mark))

(leaf eldoc-box
  :straight t
  :hook (eldoc-mode-hook . eldoc-box-hover-at-point-mode))

(leaf eldoc-overlay
  :disabled t
  :hook eldoc-mode-hook)

(leaf expand-region
  :straight t
  :bind
  ("C-;" . er/expand-region))

(leaf explain-pause-mode
  :straight t)

(leaf files
  :tag "builtin" "files"
  :custom (find-file-visit-truename . t))

(leaf unfill
  :straight t
  :bind
  ("M-q" . unfill-toggle))

(leaf visual-fill-column
  :straight t
  :commands maybe-adjust-visual-fill-column
  :hook
  ((visual-line-mode-hook visual-fill-column-mode-hook) . maybe-adjust-visual-fill-column)
  :config
  (defun maybe-adjust-visual-fill-column nil
    "Readjust visual fill column when the global font size is modified.\nThis is helpful for writeroom-mode, in particular."
    (if visual-fill-column-mode
        (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
      (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t))))

(leaf color-identifiers-mode
  :straight t
  :hook prog-mode-hook)

(leaf hl-line
  :hook (after-init-hook . global-hl-line-mode))

(leaf highlight-indent-guides
  :straight t
  :hook (prog-mode-hook-hook text-mode-hook-hook org-mode-hook-hook)
  :custom
  (highlight-indent-guides-responsive . nil)
  (highlight-indent-guides-delay      . 0.5))

(leaf rainbow-mode
  :straight t
  :hook (after-init-hook
         text-mode-hook
         org-mode-hook
         css-mode-hook
         html-mode-hook
         prog-mode-hook)
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'rainbow-mode)))

(leaf rainbow-identifiers
  :straight t
  :hook (after-init-hook
         text-mode-hook
         org-mode-hook
         css-mode-hook
         html-mode-hook
         prog-mode-hook)
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'rainbow-identifiers-mode)))

(leaf htmlize
  :straight t
  :custom (htmlize-pre-style . t))

(leaf indent-tools
  :straight t
  :bind (("C-c TAB" . indent-tools-hydra/body)))

(leaf mmm-auto
  :custom
  (mmm-global-mode
   . 'buffers-with-submode-classes)
  (mmm-submode-decoration-level
   . 2))

(leaf page-break-lines
  :straight t
  :hook ((after-init-hook . global-page-break-lines-mode))
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'page-break-lines-mode)))

(leaf paren
  :hook (after-init-hook . show-paren-mode))

(leaf rainbow-delimiters
  :straight t
  :hook prog-mode-hook org-src-mode-hook)

(leaf smartparens
  :straight t
  :hook (after-init-hook . smartparens-global-mode)
  :custom (sp-hybrid-kill-entire-symbol . nil))

(leaf quick-peek
  :straight t
  :custom-face
  (quick-peek-border-face  . '((t (:background "#75b79e" :height 0.1))))
  (quick-peek-padding-face . '((t (:height 0.1)))))

(leaf subword
  :hook prog-mode-hook)

(leaf symbol-overlay
  :straight t
  :bind (("M-i"  . symbol-overlay-put)
         ("M-n"  . symbol-overlay-switch-forward)
         ("M-p"  . symbol-overlay-switch-backward)
         ("<f8>" . symbol-overlay-remove-all)
         ("<f7>" . symbol-overlay-mode)))

(straight-register-package
 '(point-history :type git
                 :host github
                 :repo "blue0513/point-history"))

(leaf point-history
  :straight t
  :hook after-init-hook
  :bind
  ("C-c C-/" . point-history-show)
  :custom (point-history-ignore-buffer . "^ \\*Minibuf\\|^ \\*point-history-show*"))

(leaf undo-propose
  :bind
  ("C-c u" . undo-propose)
  :config
  (undo-propose-wrap undo)
  (undo-propose-wrap redo))

(leaf uniquify
  :custom
  (uniquify-buffer-name-style   . 'reverse)
  (uniquify-separator           . " • ")
  (uniquify-after-kill-buffer-p . t)
  (uniquify-ignore-buffers-re   . "^\\*"))

(leaf vlf
  :commands ffap-vlf
  :init
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

(leaf whitespace
  :disabled t
  :straight t
  :init
  (defun no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq show-trailing-whitespace nil))
  :custom
  (whitespace-line-column . 76)
  (whitespace-style
   . '(face spaces tabs space-mark tab-mark empty))
  :hook
  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  ((picture-mode-hook
    special-mode-hook
    Info-mode-hook
    eww-mode-hook
    minibuffer-setup-hook
    fundamental-mode-hook) . no-trailing-whitespace)
  (after-init-hook . global-whitespace-mode))

(leaf writeroom-mode
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    nil " Prose" nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (set (make-local-variable 'blink-cursor-interval) 0.6)
          (set (make-local-variable 'show-trailing-whitespace) nil)
          (set (make-local-variable 'line-spacing) 0.2)
          (set (make-local-variable 'electric-pair-mode) nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))

(provide 'init-editor)
;;; init-editor.el ends here
