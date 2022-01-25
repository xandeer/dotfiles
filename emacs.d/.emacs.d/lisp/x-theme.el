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

(x/append-init-hook 'ns-auto-titlebar-set-all-frames)
(add-hook 'after-make-frame-functions 'ns-auto-titlebar-set-frame)
(advice-add 'frame-set-background-mode :after 'ns-auto-titlebar-set-frame)

(defun x--set-font (frame)
  "Xandeer set font for `FRAME'."
  (when (display-graphic-p)
    (set-face-attribute
     'default nil
     :font (font-spec
            :name   "Consola Mono"
            :weight 'normal
            :size   14))

    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec
                         :name   "PingFang SC"
                         :weight 'normal
                         :size   14)
                        frame
                        'prepend))

    ;; For NS/Cocoa
    (set-fontset-font
     t
     'symbol
     (font-spec :family "Apple Color Emoji")
     frame
     'append)))

(defun x/set-font (&rest _)
  "Xandeer set font."
  (interactive)
  (x--set-font nil))

(require-package 'doom-themes)
(x/append-init-hook (lambda ()
                      (require 'doom-themes)
                      (load-theme 'x-vibrant t)))

(with-eval-after-load 'doom-theme
  (with-eval-after-load 'org-mode
    (doom-themes-org-config)))

(x/append-init-hook 'x/set-font)
(add-hook 'after-make-frame-functions   #'x/set-font)
(add-hook 'server-after-make-frame-hook #'x/set-font)

(require-package 'default-text-scale)
(x/append-init-hook #'default-text-scale-mode)

(provide 'x-theme)
;;; x-theme.el ends here
