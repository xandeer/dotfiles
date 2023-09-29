;;; x-osx-misc.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/get-bundle-id (app-name)
  "Get the bundle ID for the `APP-NAME'."
  (let ((command (format "osascript -e 'id of app \"%s\"'" app-name)))
    (string-trim (shell-command-to-string command))))

;;;###autoload
(defun x/get-bundle-id-from-application-directory ()
  "Get the bundle ID for the selected application from the Applications directory."
  (interactive)
  (let* ((app-directory "/Applications")
         (app-list (mapcar #'file-name-nondirectory (directory-files app-directory t ".app$")))
         (app-name (completing-read "Select an application: " app-list)))
    (message (x/get-bundle-id app-name))))

;;;###autoload
(defun x/insert-bundle-id ()
  "Insert the bundle id."
  (interactive)
  (insert
   (x/get-bundle-id-from-application-directory)))

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

;;;###autoload
(x--open-with open-in-default-program)

;;;###autoload
(x--open-with reveal-in-finder "Finder" default-directory)

;;;###autoload
(x--open-with reveal-project-in-finder
              "Finder"
              (or (projectile-project-root)
                  default-directory))

;;;###autoload
(x--open-with open-in-kitty "kitty" default-directory)

(provide 'x-osx-misc)
;;; x-osx-misc.el ends here
