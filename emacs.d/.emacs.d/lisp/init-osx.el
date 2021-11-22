;;; init-osx.el --- init-osx -*- lexical-binding: t -*-
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
(defun +macos-open-with (&optional app-name path)
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
(defmacro +macos--open-with (id &optional app dir)
  `(defun ,(intern (format "+macos/%s" id)) ()
     (interactive)
     (+macos-open-with ,app ,dir)))

;;;###autoload (autoload '+macos/open-in-default-program "lisp/init-osx" nil t)
(+macos--open-with open-in-default-program)

;;;###autoload (autoload '+macos/reveal-in-finder "lisp/init-osx" nil t)
(+macos--open-with reveal-in-finder "Finder" default-directory)

;;;###autoload (autoload '+macos/reveal-project-in-finder "lisp/init-osx" nil t)
(+macos--open-with reveal-project-in-finder "Finder"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload '+macos/send-to-transmit "lisp/init-osx" nil t)
(+macos--open-with send-to-transmit "Transmit")

;;;###autoload (autoload '+macos/send-cwd-to-transmit "lisp/init-osx" nil t)
(+macos--open-with send-cwd-to-transmit "Transmit" default-directory)

;;;###autoload (autoload '+macos/send-to-launchbar "lisp/init-osx" nil t)
(+macos--open-with send-to-launchbar "LaunchBar")

;;;###autoload (autoload '+macos/send-project-to-launchbar "lisp/init-osx" nil t)
(+macos--open-with send-project-to-launchbar "LaunchBar"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload '+macos/open-in-iterm "lisp/init-osx" nil t)
(+macos--open-with open-in-iterm "iTerm" default-directory)
(provide 'init-osx)
;;; init-osx.el ends here
