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
  ((xr-vibrant-brighter-comments
    xr-vibrant-comment-bg) . t)
  :config
  (defun xr/load-theme ()
    "Xandeer load theme function."
    (load-theme 'xr-vibrant t))

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
  :init
  (setq-default mode-line-format nil)
  :custom
  (awesome-tray-active-modules
   . '("buffer-name"
       "rime-active"
       "rime-inactive"
       "location"
       "mode-name"
       "git"
       "date"))
  :config
  (awesome-tray-mode 1)

  (defun xr/rime-active-info ()
    (if (and (equal current-input-method "rime")
             (bound-and-true-p rime-mode)
             (rime--should-enable-p)
             (not (rime--should-inline-ascii-p)))
        rime-title
      ""))

  (defun xr/rime-inactive-info ()
    (if (and (equal current-input-method "rime")
             (bound-and-true-p rime-mode)
             (or (not (rime--should-enable-p))
                 (rime--should-inline-ascii-p)))
        rime-title
      ""))

  (defface xr/rime-active-face
    '((((background light))
       :foreground "#0061cc" :bold t)
      (t
       :foreground "#007aff" :bold t))
    "Face for rime active in awesome tray")

  (defface xr/rime-inactive-face
    '((((background light))
       :foreground "#717175" :bold t)
      (t
       :foreground "#8e8e93" :bold t))
    "Face for rime inactive in awesome tray")

  (add-to-list 'awesome-tray-module-alist
               '("rime-active" . (xr/rime-active-info xr/rime-active-face)))
  (add-to-list 'awesome-tray-module-alist
               '("rime-inactive" . (xr/rime-inactive-info xr/rime-inactive-face)))

  ;; Override to make it use selected frame's width
  (defun awesome-tray-get-frame-width ()
    "Only calculating a main Frame width, to avoid wrong width when new frame, such as `snails'."
    (with-selected-frame (selected-frame) ;(car (last (frame-list)))
      (frame-width))))

(leaf default-text-scale
  :straight t
  :commands default-text-scale-mode
  :hook after-init-hook)

(provide 'init-theme)
;;; init-theme.el ends here
