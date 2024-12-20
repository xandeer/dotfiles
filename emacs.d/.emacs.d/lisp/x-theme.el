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

;;; fonts
(defcustom x/defalut-font "Latin Modern Mono 16"
  "Default font."
  :type 'string)

(defcustom x/cjk-font "Weibei SC"
  "Default cjk font."
  :type 'string)

(defun x/setup-fonts ()
  (set-face-attribute 'default nil :font x/defalut-font)

  (dolist (charset '(han kana cjk-misc))
    (set-fontset-font t charset x/cjk-font))
  (set-fontset-font t 'symbol "FiraCode Nerd Font Mono")

  ;; weather
  (set-fontset-font t '(#x26c5 . #x26c5) "Apple Color Emoji")
  (set-fontset-font t '(#x26c8 . #x26c8) "Apple Color Emoji")
  (set-fontset-font t '(#x1f324 . #x1f32b) "Apple Color Emoji")

  (set-fontset-font t '(#x1f5fa . #x1f6cb) "Apple Color Emoji")

  (set-fontset-font t '(#x2713 . #x2713) "Arial Unicode MS")
  ;; (add-to-list 'face-font-rescale-alist `(,unicode-font . 0.5))
  )
(add-hook 'after-init-hook #'x/setup-fonts)
;; (add-hook 'after-make-frame-functions   #'x/setup-fonts)
;; (add-hook 'server-after-make-frame-hook #'x/setup-fonts)

(add-hook 'after-init-hook #'default-text-scale-mode)

(setq custom-theme-directory (expand-file-name "theme" user-emacs-directory))

;;; doom-theme
(add-hook 'after-init-hook
          (lambda ()
            (require 'doom-themes)
            (load-theme 'doom-one-light t)))

(with-eval-after-load 'doom-theme
  (with-eval-after-load 'org-mode
    (doom-themes-org-config)))

(defun x/theme-light? ()
  "Whether the theme is light."
  (eq 'light (frame-parameter nil 'background-mode)))

(provide 'x-theme)
;;; x-theme.el ends here
