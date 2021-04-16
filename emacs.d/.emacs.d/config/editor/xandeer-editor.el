;;; xandeer-core.el --- Xandeer's Emacs Configuration editor file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor.

;;; Code:

(straight-register-package
 '(tree-sitter :host github
               :repo "ubolonton/emacs-tree-sitter"
               :files ("lisp/*.el" "langs/*.el" "langs/queries")))

(straight-register-package
 '(point-history :type git
                 :host github
                 :repo "blue0513/point-history"))

(xandeer/s-u-p
  diff-hl
  dired-hacks
  easy-kill
  *eldoc-use*
  explain-pause-mode
  expand-region
  ;; highlight
  color-identifiers-mode
  ;; hl-line  builtin
  highlight-indent-guides
  rainbow-mode
  ;; helm

  helpful
  htmlize
  indent-tools

  list-unicode-display
  mmm-mode
  multiple-cursors
  page-break-lines
  ;; paren
  smartparens
  rainbow-delimiters

  point-history
  ;; pretty-mode
  quick-peek
  symbol-overlay

  tree-sitter
  abridge-diff
  forge
  gist
  git-blamed
  git-gutter
  git-messenger
  git-modes
  git-timemachine
  magit
  magit-org-todos
  magit-todos)

(xandeer/s-u-p ansi-color)
(leaf ansi-color
  :doc "ansi-color.el translates ANSI SGR (Select Graphic Rendition) escape sequences
with face colours, bold, etc."
  :url "https://www.emacswiki.org/emacs/AnsiColor"
  :tag "comm" "processes" "terminals" "services"
  :commands colourise-compilation-buffer
  :hook ((compilation-filter-hook . colourise-compilation-buffer))
  :config
  (eval-and-compile
    (defun colourise-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-cOLOR-APPLY-on-region compilation-filter-start
                                    (point-max))))))

(leaf jka-cmpr
  :tag "builtin"
  :hook (after-init-hook . auto-compression-mode))

(straight-register-package
 '(auto-save :host github
             :repo "manateelazycat/auto-save"))
(straight-use-package 'auto-save)
(leaf auto-save
  :require t
  :config
  (setq auto-save-silent t)
  (setq auto-save-delete-trailing-whitespace t)
  (setq auto-save-idle 1)
  (add-hook 'org-capture-mode-hook 'auto-save-disable)
  (add-hook 'org-capture-prepare-finalize-hook 'auto-save-enable)
  (auto-save-enable))

(xandeer/s-u-p dash)
(leaf dash
  :doc "A modern list library for Emacs."
  :url "https://github.com/magnars/dash.el"
  :tag "lists"
  :defer-config
  (dash-enable-font-lock))

(leaf xandeer-editor-dired
  :require t)

(leaf easy-kill
  :doc "Kill & Mark Things Easily in Emacs."
  :url "https://github.com/leoliu/easy-kill"
  :tag "killing" "convenience"
  :bind (([remap kill-ring-save]
          . easy-kill)
         ([remap mark-sexp]
          . easy-mark)))

(leaf eldoc-box
  :doc "This package displays ElDoc documentations in a childframe."
  :url "https://github.com/casouri/eldoc-box"
  :tag "extensions"
  :when (eq *eldoc-use* 'eldoc-box)
  :hook ((eldoc-mode-hook . eldoc-box-hover-mode)
         (eldoc-mode-hook . eldoc-box-hover-at-point-mode)))

(leaf eldoc-overlay
  :doc "Display eldoc doc with contextual documentation overlay for easy to look."
  :url "https://github.com/stardiviner/eldoc-overlay"
  :tag "extensions"
  :when (eq *eldoc-use* 'eldoc-overlay)
  :hook eldoc-mode-hook)

(leaf expand-region
  :doc "Emacs extension to increase selected region by semantic units."
  :url "https://github.com/magnars/expand-region.el"
  :tag "marking region"
  :bind
  ("C-=" . er/expand-region))

(leaf explain-pause-mode
  :doc "top, but for Emacs."
  :url "https://github.com/lastquestion/explain-pause-mode"
  :tag "performance" "speed" "config")

(leaf files
  :tag "builtin" "files"
  :custom (find-file-visit-truename . t))

(leaf fill
  :doc "Filling text."
  :tag "emacs")

(straight-use-package 'unfill)
(leaf unfill
  :doc "Functions providing the inverse of Emacs' fill-paragraph and fill-region"
  :url "https://github.com/purcell/unfill"
  :tag "convenience"
  :bind (("M-q" . unfill-toggle)))

(straight-use-package 'visual-fill-column)
(leaf visual-fill-column
  :doc "Emacs mode for wrapping visual-line-mode buffers at fill-column."
  :url "https://github.com/joostkremers/visual-fill-column"
  :tag "convenience"
  :commands maybe-adjust-visual-fill-column
  :hook (visual-line-mode-hook
         (visual-fill-column-mode-hook . maybe-adjust-visual-fill-column))
  :config
  (defun maybe-adjust-visual-fill-column nil
    "Readjust visual fill column when the global font size is modified.\nThis is helpful for writeroom-mode, in particular."
    (if visual-fill-column-mode
        (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
      (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t))))

(leaf color-identifiers-mode
  :doc "Emacs minor mode to highlight each source code identifier uniquely based
on its name."
  :url "https://github.com/ankurdave/color-identifiers-mode"
  :tag "faces" "languages"
  :hook prog-mode-hook)

(leaf hl-line
  :doc "Highlight the current line of characters."
  :url "https://www.emacswiki.org/emacs/HighlightCurrentLine"
  :tag "faces" "frames" "emulations"
  :hook ((after-init-hook . global-hl-line-mode)))

(leaf highlight-indent-guides
  :doc "Emacs minor mode to highlight indentation."
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :tag "faces"
  :when *highlight-indent-guides*
  :hook (prog-mode-hook-hook text-mode-hook-hook org-mode-hook-hook)
  :config
  (highlight-indent-guides-responsive . nil)
  (highlight-indent-guides-delay      . 0.5))

(leaf rainbow-mode
  :doc "Colorize color names in buffers."
  :url "https://elpa.gnu.org/packages/rainbow-mode.html"
  :tag "faces"
  :hook (after-init-hook
         text-mode-hook
         org-mode-hook
         css-mode-hook
         html-mode-hook
         prog-mode-hook)
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'rainbow-mode)))

(leaf helpful
  :doc "A better Emacs *help* buffer."
  :url "https://github.com/Wilfred/helpful"
  :tag "help" "lisp"
  :bind (("C-c C-d" . helpful-at-point)))
         ;; Use ivy instead
         ;; ([remap describe-function]   . helpful-callable)
         ;; ([remap describe-variable]   . helpful-variable)
         ;; ([remap describe-bindings]   . helpful-key)))

(leaf htmlize
  :doc "Convert buffer text and decorations to HTML."
  :url "https://github.com/hniksic/emacs-htmlize"
  :tag "hypermedia" "extensions"
  :custom (htmlize-pre-style . t))

(leaf indent-tools
  :doc "Emacs mode to indent, navigate around and act on indentation units:
perfect for yaml, python and the like."
  :url "https://gitlab.com/emacs-stuff/indent-tools"
  :tag "indentation" "navigation"
  :bind (("C-c TAB" . indent-tools-hydra/body)))

(leaf list-unicode-display
  :doc "Search for and list unicode characters in Emacs.

`list-unicode-display'"
  :url "https://github.com/purcell/list-unicode-display"
  :tag "convenience")

(leaf mmm-auto
  :doc "MMM Mode is a minor mode for Emacs that allows Multiple Major Modes
to coexist in one buffer."
  :url "https://github.com/purcell/mmm-mode"
  :tag "convenience" "faces" "languages" "tools"
  :custom
  (mmm-global-mode
   . 'buffers-with-submode-classes)
  (mmm-submode-decoration-level
   . 2))

(leaf multiple-cursors
  :doc "Multiple cursors for Emacs."
  :url "https://github.com/magnars/multiple-cursors.el"
  :tag "editing" "cursors"
  :bind
  (("C-<"     . mc/mark-previous-like-this)
   ("C->"     . mc/mark-next-like-this)
   ("C-+"     . mc/mark-next-like-this)
   ("C-c C-<" . mc/mark-all-like-this)

   ("C-c m r" . set=rectangular-region-anchor)
   ("C-c m c" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)))

(leaf page-break-lines
  :doc "Emacs: display ugly ^L page breaks as tidy horizontal lines"
  :url "https://github.com/purcell/page-break-lines"
  :tag "convenience" "faces"
  :hook ((after-init-hook . global-page-break-lines-mode))
  :defer-config
  (when (fboundp 'diminish)
    (diminish 'page-break-lines-mode)))

(leaf paren
  :hook (after-init-hook . show-paren-mode))

(leaf rainbow-delimiters
  :doc "Emacs rainbow delimiters mode"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :tag "convenience" "faces" "lisp" "tools"
  :hook prog-mode-hook org-src-mode-hook)

(leaf smartparens-config
  :doc "Minor mode for Emacs that deals with parens pairs and tries to be smart about it."
  :url "https://github.com/Fuco1/smartparens"
  :tag "abbrev" "convenience" "editing"
  :hook (after-init-hook . smartparens-global-mode)
  :custom (sp-hybrid-kill-entire-symbol . nil))

(leaf quick-peek
  :doc "An inline pop-up library for Emacs Lisp."
  :url "https://github.com/cpitclaudel/quick-peek"
  :tag "convenience" "docs" "help" "tools"
  :custom-face
  (quick-peek-border-face  . '((t (:background "#75b79e" :height 0.1))))
  (quick-peek-padding-face . '((t (:height 0.1)))))

(leaf subword
  :tag "builtin"
  :hook prog-mode-hook)

(leaf symbol-overlay
  :doc "Highlight symbols with keymap-enabled overlays."
  :url "https://github.com/wolray/symbol-overlay"
  :tag "faces" "matching"
  :bind (("M-i"  . symbol-overlay-put)
         ("M-n"  . symbol-overlay-switch-forward)
         ("M-p"  . symbol-overlay-switch-backward)
         ("<f8>" . symbol-overlay-remove-all)
         ("<f7>" . symbol-overlay-mode)))

(leaf tree-sitter
  :doc "emacs-tree-sitter is an Emacs binding for tree-sitter, an incremental parsing
system."
  :url "https://github.com/ubolonton/emacs-tree-sitter"
  :tag "languagues" "parsers" "tools"
  :hook (agda-mode-hook
         shell-mode-hook
         c-mode-hook
         c++-mode-hook
         css-mode-hook
         haskell-mode-hook
         html-mode-hook
         js-mode-hook
         js2-mode-hook
         son-mode-hook
         python-mode-hook
         ruby-mode-hook
         rust-mode-hook
         typescript-mode-hook)
  :config
  (require 'tree-sitter-langs)
  (add-to-list 'tree-sitter-major-mode-language-alist '(mhtml-mode . html)))

(leaf tree-sitter-hl
  :hook tree-sitter-after-on-hook)

(leaf undo/redo
  :doc "Undo/redo for emacs"
  :tag "editing")

(leaf point-history
  :doc "Show the history of points you visited before."
  :url "https://github.com/blue0513/point-history"
  :tag "editing"
  :hook after-init-hook
  :bind (("C-c C-/" . point-history-show))
  :custom (point-history-ignore-buffer . "^ \\*Minibuf\\|^ \\*point-history-show*"))

(leaf undo-propose
  :doc "Navigate the emacs undo history by staging undo's in a temporary buffer."
  :url "https://github.com/jackkamm/undo-propose-el"
  :tag "convenience" "files" "undo" "redo" "history"
  :bind (("C-c u" . undo-propose))
  :config
  (undo-propose-wrap undo)
  (undo-propose-wrap redo))

(leaf uniquify
  :tag "builtin" "files"
  :custom
  (uniquify-buffer-name-style   . 'reverse)
  (uniquify-separator           . " • ")
  (uniquify-after-kill-buffer-p . t)
  (uniquify-ignore-buffers-re   . "^\\*"))

(leaf version-control
  :tag "git" "tools" "vc")

(leaf abridge-diff
  :doc "Emacs package for refining diff hunks with very long lines (as in LaTeX files)."
  :url "https://github.com/jdtsmith/abridge-diff"
  :tag "diffs" "magit" "tools"
  :after magit
  :init (abridge-diff-mode 1))

(leaf diff-hl
  :doc "Emacs package for highlighting uncommitted changes"
  :url "https://github.com/dgutov/diff-hl"
  :tag "vc" "diff"
  :hook ((dired-mode-hook         . diff-hl-dired-mode)
         (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(leaf forge
  :doc "Work with Git forges from the comfort of Magit."
  :url "https://github.com/magit/forge"
  :tag "git" "tools" "vc")

(leaf git-gutter
  :hook (after-init . global-git-gutter-mode)
  :custom ((git-gutter:visual-line    . t)
           (git-gutter:disabled-modes . '(asm-mode image-mode))
           (git-gutter:modified-sign  . "❚")
           (git-gutter:added-sign     . "✚")
           (git-gutter:deleted-sign   . "✘"))
  :bind (("C-x v =" . git-gutter:popup-hunk)
         ("C-x p"   . git-gutter:previous-hunk)
         ("C-x n"   . git-gutter:next-hunk)))

(leaf git-messenger
  :doc "git-messenger.el provides function that popup commit message at current line."
  :url "https://github.com/emacsorphanage/git-messenger"
  :tag "convenience" "vc"
  :custom (git-messenger:show-detail . t)
  :bind (:vc-prefix-map
         ("p" . git-messenger:popup-message)))

(leaf magit
  :doc "It's Magit! A Git porcelain inside Emacs."
  :url "https://github.com/magit/magit"
  :tag "git" "tools" "vc"
  :commands magit-status
  :hook ((magit-popup-mode-hook . no-trailing-whitespace))
  :custom ((magit-diff-refine-hunk . t)
           (vc-handled-backends    . nil))
  :bind (([(meta f12)] . magit-status)  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
         ("C-x g"      . magit-status)
         ("C-x M-g"    . magit-dispatch-popup)
         (:magit-status-mode-map
          ("C-M-<up>"  . magit-section-up))
         (:vc-prefix-map
          ("f"         . vc-git-grep))))

(leaf magit-org-todos
  :doc "Get `todo.org` into your magit status"
  :url "https://github.com/danielma/magit-org-todos.el"
  :tag "magit" "orgmode" "tools"
  :after magit
  :require t
  :config (magit-org-todos-autoinsert))

(leaf magit-todos
  :doc "Show source files' TODOs (and FIXMEs, etc) in Magit status buffer."
  :url "https://github.com/alphapapa/magit-todos"
  :tag "magit" "vc"
  :custom (magit-todos-exclude-globs . '("*.map")))

(leaf vlf
  :doc "View Large Files in Emacs"
  :url "https://github.com/m00natic/vlfi"
  :tag "files" "large files" "utilities"
  :commands ffap-vlf
  :init
  (defun ffap-vlf ()
    "Find file at point with VLF."
    (interactive)
    (let ((file (ffap-file-at-point)))
      (unless (file-exists-p file)
        (error "File does not exist: %s" file))
      (vlf file))))

(straight-use-package 'whitespace)
(leaf whitespace
  :tag "data" "wp"
  :init
  (defun no-trailing-whitespace ()
    "Turn off display of trailing whitespace in this buffer."
    (setq show-trailing-whitespace nil))
  :hook
  ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
  ((artist-mode-hook
    picture-mode-hook
    special-mode-hook
    Info-mode-hook
    eww-mode-hook
    term-mode-hook
    vterm-mode-hook
    comint-mode-hook
    compilation-mode-hook
    twittering-mode-hook
    minibuffer-setup-hook
    fundamental-mode) . no-trailing-whitespace))

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

(straight-use-package 'yasnippet)
(leaf yasnippet
  :doc "A template system for Emacs"
  :url "https://github.com/joaotavora/yasnippet"
  :tag "convenience" "enmulation"
  :commands (yas-minor-mode)
  :hook (((prog-mode-hook text-mode-hook) . yas-minor-mode))
  :config
  (add-to-list 'yas-snippet-dirs
               (concat user-emacs-directory "extra/snippets"))
  (yas-reload-all))

(defun xandeer/convert-chinese-quotations ()
  "Convert all [“|“] to [『|』] in current buffer."
  (interactive)

  (goto-char (point-min))
  (while (re-search-forward "“" nil t)
    (replace-match "「"))

  (goto-char (point-min))
  (while (re-search-forward "”" nil t)
    (replace-match "」")))

(provide 'xandeer-editor)
;;; xandeer-editor.el ends here
