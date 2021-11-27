;;; xr-osx.el --- xr-osx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
(setq mac-right-command-modifier 'hyper)
(setq mac-function-modifier 'super)

(setq locate-command "mdfind")

(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

(require-package 'osx-trash)

(setq delete-by-moving-to-trash t)
(autoload #'osx-trash-move-file-to-trash "osx-trash" nil t)

(unless (fboundp 'system-move-file-to-trash)
  (defalias #'system-move-file-to-trash #'osx-trash-move-file-to-trash))

;; https://github.com/hlissner/doom-emacs/blob/develop/modules/os/macos/autoload.el
;;;###autoload
(defun xr-open-with (&optional app-name path)
  "Send PATH to APP-NAME on OSX."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "open %s"
                          (if app-name
                              (format "-a %s '%s'" (shell-quote-argument app-name) path)
                            (format "'%s'" path)))))
    (message "Running: %s" command)
    (shell-command command)))

;;;###autoload
(defmacro xr--open-with (id &optional app dir)
  `(defun ,(intern (format "xr-%s" id)) ()
     (interactive)
     (xr-open-with ,app ,dir)))

;;;###autoload (autoload 'xr-open-in-default-program "lisp/xr-osx" nil t)
(xr--open-with open-in-default-program)

;;;###autoload (autoload 'xr-reveal-in-finder "lisp/xr-osx" nil t)
(xr--open-with reveal-in-finder "Finder" default-directory)

;;;###autoload (autoload 'xr-reveal-project-in-finder "lisp/xr-osx" nil t)
(xr--open-with reveal-project-in-finder "Finder"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload 'xr-send-to-transmit "lisp/xr-osx" nil t)
(xr--open-with send-to-transmit "Transmit")

;;;###autoload (autoload 'xr-send-cwd-to-transmit "lisp/xr-osx" nil t)
(xr--open-with send-cwd-to-transmit "Transmit" default-directory)

;;;###autoload (autoload 'xr-send-to-launchbar "lisp/xr-osx" nil t)
(xr--open-with send-to-launchbar "LaunchBar")

;;;###autoload (autoload 'xr-send-project-to-launchbar "lisp/xr-osx" nil t)
(xr--open-with send-project-to-launchbar "LaunchBar"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload 'xr-open-in-iterm "lisp/xr-osx" nil t)
(xr--open-with open-in-iterm "iTerm" default-directory)
(provide 'xr-osx)
;;; xr-osx.el ends here
