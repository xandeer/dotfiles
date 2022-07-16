;;; x-osx.el --- x-osx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
(setq mac-right-command-modifier 'hyper)
(setq mac-function-modifier 'super)

(setq locate-command "mdfind")

(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

(setq delete-by-moving-to-trash t)
(autoload #'osx-trash-move-file-to-trash "osx-trash" nil t)

(unless (fboundp 'system-move-file-to-trash)
  (defalias #'system-move-file-to-trash #'osx-trash-move-file-to-trash))

;;;###autoload
(defun x/open (path)
  "Send PATH to open."
  (interactive)
  (shell-command (concat "open " path)))

;; https://github.com/hlissner/doom-emacs/blob/develop/modules/os/macos/autoload.el
;;;###autoload
(defun x/open-with (&optional app-name path)
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
(defmacro x--open-with (id &optional app dir)
  `(defun ,(intern (format "x/%s" id)) ()
     (interactive)
     (x/open-with ,app ,dir)))

;;;###autoload (autoload 'x/open-in-default-program "lisp/x/osx" nil t)
(x--open-with open-in-default-program)

;;;###autoload (autoload 'x/reveal-in-finder "lisp/x/osx" nil t)
(x--open-with reveal-in-finder "Finder" default-directory)

;;;###autoload (autoload 'x/reveal-project-in-finder "lisp/x/osx" nil t)
(x--open-with reveal-project-in-finder "Finder"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload 'x/send-to-transmit "lisp/x/osx" nil t)
(x--open-with send-to-transmit "Transmit")

;;;###autoload (autoload 'x/send-cwd-to-transmit "lisp/x/osx" nil t)
(x--open-with send-cwd-to-transmit "Transmit" default-directory)

;;;###autoload (autoload 'x/send-to-launchbar "lisp/x/osx" nil t)
(x--open-with send-to-launchbar "LaunchBar")

;;;###autoload (autoload 'x/send-project-to-launchbar "lisp/x/osx" nil t)
(x--open-with send-project-to-launchbar "LaunchBar"
                   (or (doom-project-root) default-directory))

;;;###autoload (autoload 'x/open-in-iterm "lisp/x/osx" nil t)
(x--open-with open-in-iterm "iTerm" default-directory)

(provide 'x-osx)
;;; x-osx.el ends here
