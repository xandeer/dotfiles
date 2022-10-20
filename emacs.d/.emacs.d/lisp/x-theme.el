;;; x-theme.el --- x-theme -*- lexical-binding: t -*-
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

(add-hook 'after-init-hook #'ns-auto-titlebar-set-all-frames)
(add-hook 'after-make-frame-functions #'ns-auto-titlebar-set-frame)
(advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame)

(defcustom x/defalut-font "Latin Modern Mono"
  "Default font."
  :type 'string)

(defcustom x/cjk-font "Weibei SC"
  "Default cjk font."
  :type 'string)

(defcustom x/font-size 16
  "Default font size."
  :type 'number)

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames nil
        frame-resize-pixelwise t)

  (setq unicode-font "PingFang SC"
        emoji-font "Apple Color Emoji"
        symbol-font "Apple Symbols"))

(defun x/setup-fonts ()
  (set-face-attribute
   'default nil
   :font
   (font-spec :family x/defalut-font
              :size x/font-size))

  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset unicode-font)
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec
                       :name x/cjk-font
                       :weight 'bold
                       :size x/font-size)
                      (selected-frame)
                      'prepend))
  (add-to-list 'face-font-rescale-alist `(,unicode-font
                                          . 0.5))

  (set-fontset-font t 'emoji emoji-font nil 'prepend)
  (set-fontset-font t 'symbol symbol-font nil 'prepend))

(setq custom-theme-directory (expand-file-name "theme" vanilla-path))
(add-hook 'after-init-hook (lambda ()
                             (require 'doom-themes)
                             ;; (load-theme 'x-vibrant t)
                             (load-theme x/theme t)))

(with-eval-after-load 'doom-theme
  (with-eval-after-load 'org-mode
    (doom-themes-org-config)))

(add-hook 'after-init-hook #'x/setup-fonts)
(add-hook 'after-make-frame-functions   #'x/setup-fonts)
(add-hook 'server-after-make-frame-hook #'x/setup-fonts)

(add-hook 'after-init-hook #'default-text-scale-mode)

(provide 'x-theme)
;;; x-theme.el ends here
