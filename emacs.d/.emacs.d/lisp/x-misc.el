;;; x-misc.el --- misc -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/get-bundle-id (app-name)
  "Get the bundle ID for the `APP-NAME'."
  (let ((command (format "osascript -e 'id of app \"%s\"'" app-name)))
    (string-trim (shell-command-to-string command))))

(defun x/get-bundle-id-from-application-directory ()
  "Get the bundle ID for the selected application from the Applications directory."
  (interactive)
  (let* ((app-directory "/Applications")
         (app-list (mapcar #'file-name-nondirectory (directory-files app-directory t ".app$")))
         (app-name (completing-read "Select an application: " app-list)))
    (message (get-bundle-id app-name))))

(defun x/insert-bundle-id ()
  "Insert the bundle id."
  (interactive)
  (insert
   (x/get-bundle-id-from-application-directory)))

(provide 'x-misc)
;;; x-misc.el ends here
