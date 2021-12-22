;;; x-utils.el --- x-utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/replace (old new &optional beg end)
  "Replace the string OLD with string NEW.
BEG means begin point, END meas end point.
Default use `point-min` or `point-max`."
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))

  (if (> beg end)
      (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    (goto-char end)
    (insert
     (let ((buf (current-buffer)))
       (with-temp-buffer
         (switch-to-buffer (current-buffer) nil t)
         (insert-buffer-substring buf beg end)
         (goto-char (point-min))
         (while (re-search-forward old nil t)
           (replace-match new))
         (buffer-string))))
    (delete-region beg end)))

(defun x/convert-chinese-quotations ()
  "Convert all [“|“ ‘|’] to [ 「|」『|』] in current buffer."
  (interactive)

  (let ((quotas
         '(("‘" . "『")
           ("’" . "』")
           ("“" . "「")
           ("”" . "」")
           ("，" . ", ")
           ("；" . "; ")
           ("。" . ". ")
           ("？" . "? ")
           ("：" . ": ")
           ("（" . "(")
           ("）" . ")")
           ("！" . "! "))))
    (mapc (lambda (q)
            (x/replace (car q) (cdr q)))
          quotas)))

(defun x/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name))
  (kill-current-buffer))

(defun x/duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (progn
    (move-beginning-of-line 1)
    (insert (thing-at-point 'line))
    (move-end-of-line 1)))

(defun x/bookmark (name)
  "Goto bookmark with NAME, or update it."
  (interactive)
  (if (s-contains? name (buffer-name))
      (bookmark-set name)
    (bookmark-jump name)))

(defun x/kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

(defun x/expand-repo (path)
  "Expand PATH in ~/prejects/personal ."
  (expand-file-name path "~/projects/personal"))

(defun x/expand-note (path)
  "Expand PATH in `org-directory`."
  (expand-file-name path (x/expand-repo "notes")))

(global-set-key (kbd "H-e") 'x/kill-other-window-buffer)
(global-set-key (kbd "H-b") (lambda () (interactive) (switch-to-buffer "*scratch*")))

(defun x--trash (path)
  (shell-command (concat "trash " path)))

(defun x/trash-temp ()
  "Move some temp files to trash."
  (interactive)
  (dolist (path '("~/temp/screenshot/*.png"
                  "~/temp/donut/*.apk"
                  "~/temp/donut/*.zip"
                  "~/temp/donut/*.aab"
                  "~/syncthing/donut/apks/*"
                  "~/syncthing/personal/temp/*"
                  ))
    (x--trash path)))

;; Move it to /Library/LaunchDaemons
;; (add-hook #'after-init-hook
          ;; (lambda ()
            ;; (async-shell-command "~/bin/hs -d ~/Downloads" "*hs-daemon*")))

(defun x/change-hs-root (path)
  (interactive)
  (let ((url-request-method "PUT"))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://localhost"
                 (expand-file-name path)))
      (buffer-string))))

(defun x/change-hs-on-dired ()
  (interactive)
  (x/change-hs-root dired-directory))

(defun x/ifconfig ()
  (interactive)
  (message (format-network-address (car (network-interface-info "en0")) t)))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(defun x/rename (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun x/browse-current-file ()
  "Open the current file as a URL using `browse-url`."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun x--launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun x/restart-emacs ()
  "Restart Emacs."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook (list #'x--launch-separate-emacs-under-x))))
    (save-buffers-kill-emacs)))

(defun x/load-current ()
  "Load the current elisp file."
  (interactive)
  (load-file (buffer-file-name)))

;;; utils to remove leaf
(defun x/leaf-expand-custom (beg end)
  (interactive "r")
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (x/replace "\\([a-z-]*\\) *\\. " "setq \\1 " beg end))

(defun x/leaf-expand-global-bind (beg end)
  (interactive "r")
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (x/replace "\\(\".*\"\\) *\\. " "global-set-key (kbd \\1) #'" beg end))

(defun x/leaf-expand-map-bind (beg end)
  (interactive "r")
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (x/replace "\\(\".*\"\\) *\\. "
              (concat "define-key "
                      (read-from-minibuffer "Map: ")
                      " (kbd \\1) #'")
              beg end))

(autoload #'lispy-narrow "lispy" nil t)
(defun x/toggle-narrow ()
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (call-interactively #'lispy-narrow)))

(provide 'x-utils)
;;; x-utils.el ends here
