;;; init-theme.el --- init-theme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun ns-auto-titlebar-set-frame (frame &rest _)
  "Set ns-appearance frame parameter for FRAME to match its background-mode parameter."
  (when (display-graphic-p frame)
    (let ((mode (frame-parameter frame 'background-mode)))
      (modify-frame-parameters frame `((ns-transparent-titlebar . t) (ns-appearance . ,mode))))))

(defun ns-auto-titlebar-set-all-frames (&rest _)
  "Set ns-appearance frame parameter for all frames to match their background-mode parameter."
    (mapc 'ns-auto-titlebar-set-frame (frame-list)))

(add-hook 'after-init-hook 'ns-auto-titlebar-set-all-frames)
(add-hook 'after-make-frame-functions 'ns-auto-titlebar-set-frame)
(advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame)

(leaf doom-themes
  :straight t
  :hook
  (after-init-hook . xr/load-theme)
  :custom
  ((doom-dracula-brighter-comments
    doom-dracula-colorful-headers
    doom-dracula-comment-bg) . t)
  :config
  (doom-themes-visual-bell-config)

  (defun xr/load-theme ()
    "Xandeer load theme function."
    (load-theme 'doom-dracula t)
    (setq doom-modeline-minor-modes nil))

  (with-eval-after-load 'org-mode
    (doom-themes-org-config)))

(defun xr/set--font (frame)
  "Xandeer set font for `FRAME'."
  (when (display-graphic-p)
    (set-face-attribute
     'default nil
     :font (font-spec
            :name   "Consola Mono"
            :weight 'normal
            :size   16))

    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec
                         :name   "PingFang SC"
                         :weight 'normal
                         :size   16)
                        frame
                        'prepend))

    ;; For NS/Cocoa
    (set-fontset-font
     t
     'symbol
     (font-spec :family "Apple Color Emoji")
     frame
     'append)))

(defun xr/set-font (&rest _)
  "Xandeer set font."
  (interactive)
  (xr/set--font nil))

(add-hook #'after-init-hook #'xr/set-font)
(add-hook #'after-make-frame-functions   #'xr/set-font)
(add-hook #'server-after-make-frame-hook #'xr/set-font)

(straight-register-package
 '(awesome-tray :host github
                :repo "manateelazycat/awesome-tray"))
(leaf awesome-tray
  :straight t
  :custom
  (awesome-tray-active-modules
   . '("git"
       ;; "file-path"
       ;; "buffer-name"
       "location"
       "input-method"
       "mode-name"
       "date"))
  :config
  (setq-default mode-line-format nil)

  ;; Override to make it use selected frame's width
  (defun awesome-tray-get-frame-width ()
    "Only calculating a main Frame width, to avoid wrong width when new frame, such as `snails'."
    (with-selected-frame (selected-frame) ;(car (last (frame-list)))
      (frame-width)))

  (awesome-tray-mode 1))

(leaf default-text-scale
  :straight t
  :commands default-text-scale-mode
  :hook after-init-hook)

(provide 'init-theme)
;;; init-theme.el ends here
