;;; init-theme.el --- init-theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf doom-themes
  :straight t
  :custom ((doom-dracula-brighter-comments
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
     :font (font-spec :name   *font*
                      :weight *font-weight*
                      :size   *font-size*))

    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :name   *font-cjk*
                                   :weight *font-weight-cjk*
                                   :size   *font-size-cjk*)
                        frame
                        'prepend))

    (if *is-a-mac*
         ;; For NS/Cocoa
        (set-fontset-font t
                           'symbol
                           (font-spec :family "Apple Color Emoji")
                           frame
                           'append)
       ;; For Linux
      (set-fontset-font t
                        'symbol
                        (font-spec :family "Symbola")
                        frame
                        'prepend))

    (set-face-attribute 'mode-line nil
                        :font (font-spec :name   *font*
                                         :weight 'normal
                                         :size   (if *is-a-mac* 15 25)))

    (set-face-attribute 'mode-line-inactive nil
                        :font (font-spec :name   *font*
                                         :weight 'normal
                                         :size   (if *is-a-mac* 15 25)))))

(defun xr/set-font (&rest _)
  "Xandeer set font."
  (interactive)
  (xr/set--font nil))

(add-hook #'after-init-hook #'xr/set-font)
(add-hook #'after-make-frame-functions   #'xr/set-font)
(add-hook #'server-after-make-frame-hook #'xr/set-font)

(defun xr/load-theme ()
  "Xandeer load theme function"
  (when *theme*
    (load-theme *theme* t)))

(add-hook #'after-init-hook #'xr/load-theme)

(leaf minions
  :straight t
  :hook after-init-hook
  :custom (minions-mode-line-lighter . "✬"))

(leaf doom-modeline
  :straight t
  :hook after-init-hook
  :defun doom-modeline-def-segment
  :custom
  ((doom-modeline-height                      . 25)
   (doom-modeline-bar-width                   . 3)
   (doom-modeline-window-width-limit          . fill-column)
   (doom-modeline-project-detection           . 'project)
   (doom-modeline-buffer-file-name-style      . 'truncate-with-project)
   ((doom-modeline-icon
     doom-modeline-major-mode-icon
     doom-modeline-major-mode-color-icon
     doom-modeline-buffer-state-icon
     doom-modeline-buffer-modification-icon
     doom-modeline-unicode-fallback
     doom-modeline-minor-modes)
    . t)
   (doom-modeline-enable-word-count . nil)
   (doom-modeline-buffer-encoding             . nil)
   (doom-modeline-indent-info                 . nil)
   (doom-modeline-checker-simple-format       . nil)
   (doom-modeline-number-limit                . 99)
   (doom-modeline-vcs-max-length              . 12)
   (doom-modeline-persp-name                  . nil)
   (doom-modeline-display-default-persp-name  . nil)
   (doom-modeline-lsp                         . t)
   (doom-modeline-github                      . t)
   `(doom-modeline-github-interval            . ,(* 30 60))
   (doom-modeline-modal-icon                  . t)))

(provide 'init-theme)
;;; init-theme.el ends here
