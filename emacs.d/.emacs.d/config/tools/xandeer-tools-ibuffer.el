;;; xandeer-editor-ibuffer.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration ibuffer.

;;; Code:

(xandeer/s-u-p ibuffer)
(leaf ibuffer
  :doc "Ibuffer is an advanced replacement for BufferMenu, which lets you
operate on buffers much in the same manner as Dired."
  :url "https://www.emacswiki.org/emacs/IbufferMode"
  :tag "buffer" "convenience"
  :bind (("C-x C-b" . ibuffer))
  :hook ((ibuffer-mode-hook . ibuffer-switch-to-normal))
  :custom
  (ibuffer-saved-filter-groups
   . '(("Normal"
        ("Dired"      (mode . dired-mode))
        ("Emacs"     (or
                      (name . "^\\*dashboard\\*$")
                      (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")
                      (name . "^\\*Backtrace\\*$")))
        ("Term"       (mode . vterm-mode))
        ("Text"      (or
                      (mode . org-mode)
                      (mode . markdown)
                      (mode . rst-mode)
                      (mode . text-mode)))
        ("TeX"        (mode . tex-mode))
        ("Languages" (or
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
        ("GNUs"      (or
                      (mode . message-mode)
                      (mode . bbdb-mode)
                      (mode . mail-mode)
                      (mode . gnus-group-mode)
                      (mode . gnus-summary-mode)
                      (mode . gnus-article-mode)
                      (name . "^\\.bbdb$")
                      (name . "^\\.newsrc-dribble")))
        ("Magit"      (name . "^magit"))
        ("Help"      (or
                      (name . "^\\*Help\\*$")
                      (name . "^\\*Apropos\\*$")
                      (name . "^\\*info\\*$")
                      (name . "^\\*helpful")))
        ("Custom"    (or
                      (mode . custom-mode)
                      (name . "^\\*Customize")))
        ("Helm"       (mode . helm-major-mode)))))
  (ibuffer-show-empty-filter-groups . nil)
  (ibuffer-default-sorting-mode     . 'filename/process)
  :defer-config
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size)))

  (gsetq ibuffer-formats
         '((mark modified read-only vc-status-mini " "
                 (name 22 22 :left :elide)
                 " "
                 (size-h 9 -1 :right)
                 " "
                 (mode 12 12 :left :elide)
                 " "
                 vc-relative-file)
           (mark modified read-only vc-status-mini " "
                 (name 22 22 :left :elide)
                 " "
                 (size-h 9 -1 :right)
                 " "
                 (mode 14 14 :left :elide)
                 " "
                 (vc-status 12 12 :left)
                 " "
                 vc-relative-file)))
  (defun ibuffer-switch-to-normal ()
    "ibuffer swith to normal filter groups."
    (ibuffer-switch-to-saved-filter-groups "Normal")))

(xandeer/s-u-p ibuffer-vc)
(leaf ibuffer-vc
  :doc "Let Emacs' ibuffer-mode group files by git project etc., and show file state"
  :url "https://github.com/purcell/ibuffer-vc"
  :tag "convenience")

(xandeer/s-u-p all-the-icons-ibuffer)
(leaf all-the-icons-ibuffer
  :doc "Display icons for all buffers in ibuffer."
  :url "https://github.com/seagle0128/all-the-icons-ibuffer"
  :tag "convenience" "icons" "ibuffer"
  :hook after-init-hook)

(provide 'xandeer-tools-ibuffer)
;;; xandeer-tools-ibuffer.el ends here
