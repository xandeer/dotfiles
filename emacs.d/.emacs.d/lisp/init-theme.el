;;; init-theme.el --- init-theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf doom-themes
  :straight t
  :custom
  ((doom-dracula-brighter-comments
    doom-dracula-colorful-headers
    doom-dracula-comment-bg) . t)
  :config
  (with-eval-after-load 'treemacs
    (doom-themes-treemacs-config)
    (gsetq doom-themes-treemacs-theme "doom-colors"))
  (doom-themes-visual-bell-config)
  (with-eval-after-load 'org-mode
    (doom-themes-org-config)))

(defun xr/set--font (frame)
  "Xandeer set font for `FRAME'."
  (when (display-graphic-p)
    (set-face-attribute
     'default nil
     :font (font-spec
            :name   *font*
            :weight *font-weight*
            :size   *font-size*))

    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec
                         :name   *font-cjk*
                         :weight *font-weight-cjk*
                         :size   *font-size-cjk*)
                        frame
                        'prepend))


    ;; For NS/Cocoa
    (set-fontset-font t
                      'symbol
                      (font-spec :family "Apple Color Emoji")
                      frame
                      'append)

    (set-face-attribute 'mode-line nil
                        :font (font-spec
                               :name   *font*
                               :weight 'normal
                               :size   15))

    (set-face-attribute 'mode-line-inactive nil
                        :font (font-spec
                               :name   *font*
                               :weight 'normal
                               :size   15))))

(defun xr/set-font (&rest _)
  "Xandeer set font."
  (interactive)
  (xr/set--font nil))

(add-hook #'after-init-hook #'xr/set-font)
(add-hook #'after-make-frame-functions   #'xr/set-font)
(add-hook #'server-after-make-frame-hook #'xr/set-font)

(defun xr/load-theme ()
  "Xandeer load theme function."
  (when *theme*
    (load-theme *theme* t)
    (setq doom-modeline-minor-modes nil)))

(add-hook #'after-init-hook #'xr/load-theme)

(leaf minions
  :straight t
  :disabled t
  :hook after-init-hook
  :custom (minions-mode-line-lighter . "✬"))

(leaf doom-modeline
  :straight t
  :hook after-init-hook
  :defun doom-modeline-def-segment
  :custom
  (doom-modeline-height                      . 25)
  (doom-modeline-bar-width                   . 3)
  (doom-modeline-window-width-limit          . fill-column)
  (doom-modeline-project-detection           . 'project)
  (doom-modeline-buffer-file-name-style      . 'truncate-with-project)
  ((doom-modeline-icon
    doom-modeline-major-mode-icon
    doom-modeline-major-mode-color-icon
    doom-modeline-buffer-state-icon
    doom-modeline-buffer-modification-icon)
   . t)
  ((doom-modeline-segment--buffer-encoding
    doom-modeline-minor-modes
    doom-modeline-persp-name)
   . nil))

(straight-register-package
 '(svg-tag-mode
   :host github
   :branch "main"
   :repo "rougier/svg-tag-mode"
   :files ("svg-tag-mode.el")))
(leaf svg-tag-mode
  :straight t
  :require t
  :disabled t
  :config
  (defface svg-tag-note-face
    '((t :foreground "black" :background "white" :box "black"
         :family "Roboto Mono" :weight light :height 120))
    "Face for note tag" :group nil)

  (defface svg-tag-keyboard-face
    '((t :foreground "#333333" :background "#f9f9f9" :box "#333333"
         :family "Roboto Mono" :weight light :height 120))
    "Face for keyboard bindings tag" :group nil)

  (defface svg-tag-org-face
    '((t :foreground "#333333" :background "#fffff0" :box "#333333"
         :family "Roboto Mono" :weight light :height 120))
    "Face for keyboard bindings tag" :group nil)

  (setq svg-tag-todo
        (svg-tag-make "TODO" nil 1 1 2))

  (setq svg-tag-note
        (svg-tag-make "NOTE" 'svg-tag-note-face 2 0 2))

  (defun svg-tag-round (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 12))

  (defun svg-tag-quasi-round (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 8))

  (defun svg-tag-keyboard (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-keyboard-face 1 1 2))

  (defun svg-tag-org (text)
    (svg-tag-make (substring text 1 -1) 'svg-tag-org-face 1 1 2))

  (setq svg-tag-tags
        '(("@[0-9a-zA-Z]+:"               . svg-tag-org)
          ("TODO"                         . svg-tag-todo)
          ("NOTE"                         . svg-tag-note)
          (":TODO:"                       . svg-tag-todo)
          (":NOTE:"                       . svg-tag-note)
          ("\([0-9a-zA-Z]\)"              . svg-tag-round)
          ("\([0-9a-zA-Z][0-9a-zA-Z]\)"   . svg-tag-quasi-round)
          ("|[0-9a-zA-Z- ⇥></%⌘^→←↑↓]+?|" . svg-tag-keyboard)))

  (svg-tag-mode 1))

(provide 'init-theme)
;;; init-theme.el ends here
