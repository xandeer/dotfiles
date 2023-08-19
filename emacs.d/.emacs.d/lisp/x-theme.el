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
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil)
  ;; (add-to-list 'face-font-rescale-alist `(,unicode-font . 0.5))
  )
(add-hook 'after-init-hook #'x/setup-fonts)
;; (add-hook 'after-make-frame-functions   #'x/setup-fonts)
;; (add-hook 'server-after-make-frame-hook #'x/setup-fonts)

(add-hook 'after-init-hook #'default-text-scale-mode)

(setq custom-theme-directory (expand-file-name "theme" vanilla-path))

;;; doom-theme
(add-hook 'after-init-hook
          (lambda ()
            (require 'doom-themes)
            (load-theme 'doom-one-light t)))

(with-eval-after-load 'doom-theme
  (with-eval-after-load 'org-mode
    (doom-themes-org-config)))

(provide 'x-theme)
;;; x-theme.el ends here
