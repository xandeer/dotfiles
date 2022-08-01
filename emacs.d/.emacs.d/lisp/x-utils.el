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
           ("・" . "·")
           ("！" . "! ")
           (" )" . ")")
           (" 」" . "」")
           (" 』" . "』"))))
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

;; (defun x/change-hs-root (path)
;;   (interactive)
;;   (let ((url-request-method "PUT"))
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          (concat "http://localhost"
;;                  (expand-file-name path)))
;;       (buffer-string))))

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

(defun x/change-hs-on-dired ()
  (interactive)
  (x/change-hs-root dired-directory))

(defun x/ifconfig ()
  (interactive)
  (message (format-network-address
            (or
             (car (network-interface-info "en0"))
             (car (network-interface-info "en1")))
            t)))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
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

(defun x/browse-current-file ()
  "Open the current file as a URL using `browse-url`."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun x/copy-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun x/launch-separate-emacs-under-x ()
  "Launch a separate Emacs instance under X."
  (interactive)
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun x/restart-emacs ()
  "Restart Emacs."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook (list #'x/launch-separate-emacs-under-x))))
    (save-buffers-kill-emacs)))

(defun x/load-current ()
  "Load the current elisp file."
  (interactive)
  (load-file (buffer-file-name)))

(defun x/toggle-narrow (arg)
  (interactive "p")
  (if (buffer-narrowed-p)
      (widen)
    (cond ((region-active-p)
           (narrow-to-region (region-beginning) (region-end)))
          (lispy-mode
           (lispy-narrow arg))
          ((equal major-mode 'org-mode)
           (org-toggle-narrow-to-subtree))
          (smartparens-mode
           (sp-narrow-to-sexp arg)))))

;;; exercism
(defun x/exercism-submit ()
  "Submit the current buffer to Exercism."
  (interactive)
  (shell-command (format "exercism submit %s"
                         (buffer-file-name))))

(defun x/exercism-open-readme-other-window ()
  "Open the README related with current file."
  (interactive)
  (let ((root (locate-dominating-file (buffer-file-name) "README.md")))
    (find-file-other-window (expand-file-name "README.md" root))))

;;; cow
(defun x--cow (file)
  "Upload the FILE to cow."
  (x/start-process (format "%s %s"
                           (expand-file-name "~/bin/cow")
                           file)))

(defun x/cow-current ()
  "Upload the current file to cow."
  (interactive)
  (x--cow
   (if (equal major-mode 'dired-mode) (dired-get-filename)
     (buffer-file-name))))

(defun x/cow ()
  "Read a file and then upload to cow."
  (interactive)
  (x--cow
   (read-file-name "Cow Upload: " (expand-file-name "~/syncthing/"))))

(defun x/null-st ()
  "Upload the current file to 0x0.st"
  (interactive)
  (x/start-process
   (format "null.exs %s"
           (buffer-file-name))))

;;; dots
(defun x/dots ()
  "Run make with dotfiles/Makefile."
  (interactive)
  (makefile-executor-execute-target (expand-file-name "~/projects/personal/dotfiles/Makefile")))

;;; string
;; copy from https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
;; Like diminish, but for major modes
(defun x/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(autoload 'derived-mode-hook-name "derived")
(defun x/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'x/set-major-mode-name name)))

;;; keybindings
(defun x/define-keys (map bindings)
  "Define keys in MAP according to BINDINGS.

Example:
  (x/define-keys org-agenda-mode-map
                 (quote ((\"k\" . org-agenda-previous-item)
                         (\"p\" . org-agenda-previous-item)
                         (\"n\" . org-agenda-next-item)
                         (\"j\" . org-agenda-next-item)
                         (\"T\" . org-agenda-goto-today)
                         (\"i\" . org-agenda-clock-in)
                         (\"o\" . org-agenda-clock-goto))))"
  (mapc (lambda (binding)
          (define-key map (kbd (car binding)) (cdr binding)))
        bindings))

(provide 'x-utils)
;;; x-utils.el ends here
