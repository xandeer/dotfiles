;;; x-file.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun x/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name))
  (kill-current-buffer))

;;; file
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
;;;###autoload
(defun x/rename ()
  "Renames both current buffer and file it's visiting to a new name."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (when (file-exists-p filename)
      (let ((new-name (read-from-minibuffer "New name: " name)))
        (rename-file filename new-name 1)
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))

;;;###autoload
(defun x/browse-current-file ()
  "Open the current file as a URL using `browse-url`."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;;###autoload
(defun x/copy-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

;;;###autoload
(defun x/trash-temp ()
  "Move some temp files to trash."
  (interactive)
  (dolist (path '("~/syncthing/donut/apk"
                  "~/syncthing/personal/temp"
                  "~/.cache/azure-tts/"))
    (shell-command (concat "trash " path
                           "; mkdir -p " path)
                   "*x/trash-temp*")))

;;;###autoload
(defun x/change-hs-root (path)
  "Start dufs at PATH."
  (interactive)
  (let* ((program "hs")
         (buffer (x/process-buffer-get program))
         (process (get-buffer-process buffer)))
    (when process
      (kill-process process)
      (sleep-for 0.1))
    (x/start-process (format "%s %s" program path))))

;;;###autoload
(defun x/change-hs-on-dired ()
  (interactive)
  (x/change-hs-root dired-directory))

;;;###autoload
(defun x/null-st ()
  "Upload the current file to 0x0.st"
  (interactive)
  (x/start-process
   (format "null.exs %s"
           (buffer-file-name))))

;;;###autoload
(defun x/push2fs (file)
  "Push `FILE' to fs.xmind.cn."
  (interactive "f")
  (let* ((user "kevin")
         (pwd (auth-source-pick-first-password
               :host "fs.xmind.cn"
               :user user)))
    (x/start-process
     (format "push2fs.sh -u %s -p %s -f %s -d donut-apk"
             user pwd file)
     't)))

;;;###autoload
(defun x/push2fs-current ()
  "Push current file to fs.xmind.cn."
  (interactive)
  (x/push2fs (buffer-file-name)))

(provide 'x-file)
;;; x-file.el ends here
