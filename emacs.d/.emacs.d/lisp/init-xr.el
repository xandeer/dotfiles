;;; init-xr.el --- init-xr -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun xr/replace (old new &optional beg end)
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

(defun xr/convert-chinese-quotations ()
  "Convert all [“|“ ‘|’] to [ 「|」『|』] in current buffer."
  (interactive)

  (let ((quotas
         '(("‘" . "『")
           ("’" . "』")
           ("“" . "「")
           ("”" . "」"))))
    (mapc (lambda (q)
            (xr/replace (car q) (cdr q)))
          quotas)))

(defun xr/org-heading-beginning-p ()
  "Whether the point is at beginning of a heading."
  (and (org-at-heading-p) (= (line-beginning-position) (point))))

(defun xr/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name))
  (kill-current-buffer))

(defun xr/fill-subtree ()
  "Toggle fill in current subtree."
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (unfill-toggle)))

(defun xr/duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (progn
    (move-beginning-of-line 1)
    (insert (thing-at-point 'line))
    (move-end-of-line 1)))

;; https://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
(defun xr/smart-beginning-of-line ()
  "Move point to first non-whitespace character or `beginning-of-line`.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))


(defun xr/journal-date (year)
  "Generate a date on today in YEAR."
  (let ((d (split-string (string-remove-suffix ".org" (buffer-name)) "-")))
    (date-to-time (format "20%s-%s-%s +0800" year (nth 1 d) (nth 2 d)))))

(defun xr/migrate-journal ()
  "Replace journal's title."
  (interactive)
  (let ((today (xr/journal-date "20")))
    (xr/replace "^#\\+STARTUP: .*\n" "")
    (xr/clear-file-links)
    (xr/replace "^\\(#\\+TITLE: \\).*" (format-time-string "\\1%B %m-%d" today))
    (xr/replace "^\\* .*" (format-time-string "* %B %d\n** %Y" today))
    (search-forward "** 2020")
    (org-set-tags (format-time-string ":%a:" today))
    (xr/replace "^\\*" "**" (point))))

(defun xr/insert-journal-in-year (year)
  "Insert a journal heading like: ** YEAR :Mon:."
  (interactive "sYear default[18], j[21], k[16], else[17]: ")
  (setq year (cond ((s-blank-str? year) 18)
                   ((s-equals? year "k") 16)
                   ((s-equals? year "j") 21)
                   (t 17)))
  (goto-char (point-min))
  (let ((today (xr/journal-date year)))
    (if (= year 21) (progn
                      (search-forward (format-time-string "* %B %d" today))
                      (newline))
      (goto-char (point-max)))
    (insert (format-time-string "** %Y" today))
    (org-set-tags (concat (format-time-string ":%a:" today)
                          (when (= year 21) "@home:"))))
  (end-of-line)
  (newline))



(defun xr/bookmark (name)
  "Goto bookmark with NAME, or update it."
  (interactive)
  (if (s-contains? name (buffer-name))
      (bookmark-set name)
    (bookmark-jump name)))


(defun xr/clear-file-link-at-point ()
  (save-excursion
    (save-match-data
      (let* ((link (org-element-context))
             (type (org-element-property :type link))
             (path (org-element-property :path link))
             (desc (and (org-element-property :contents-begin link)
                        (org-element-property :contents-end link)
                        (buffer-substring-no-properties
                         (org-element-property :contents-begin link)
                         (org-element-property :contents-end link)))))
        (goto-char (org-element-property :begin link))
        (when (and (org-in-regexp org-link-any-re 1)
                   (string-equal type "file"))
          (replace-match (or desc path)))))))

(defun xr/clear-file-links ()
  "Clear the old file links."
  (interactive)
  (org-with-point-at 1
    (while (re-search-forward org-link-bracket-re nil t)
      (xr/clear-file-link-at-point))))

(defun xr/remove-links-forward ()
  "Remove links after current point."
  (interactive)
  (xr/remove-links (point) (point-max)))

(defun xr/remove-links-backward ()
  "Remove links before current point."
  (interactive)
  (xr/remove-links (point-min) (point)))

(defun xr/remove-links-in-buffer ()
  "Remove links in the current buffer."
  (interactive)
  (xr/remove-links (point-min) (point-max)))

(defun xr/remove-links (beg end)
  "Remove links between BEG and END."
  (interactive "r")
  (unless beg (setq beg (point-min)))
  (unless end (setq end (point-max)))
  (xr/replace "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" beg end))


(defun xr/kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

(defun xr/expand-note (path)
  "Expand PATH in `org-directory`."
  (expand-file-name path org-directory))

(global-set-key (kbd "H-e") 'xr/kill-other-window-buffer)
(global-set-key (kbd "H-b") (lambda () (interactive) (switch-to-buffer "*scratch*")))

(defun xr/trash (path)
  (shell-command (concat "trash " path)))

(defun xr/trash-temp ()
  "Move some temp files to trash."
  (interactive)
  (dolist (path '("~/temp/screenshot/*.png"
                  "~/temp/donut/*.apk"
                  "~/temp/donut/*.zip"
                  "~/temp/donut/*.aab"
                  ))
    (xr/trash path)))


(defvar xr/auto-timer nil)

(defun xr/auto-session ()
  (eva-query-mood)
  (when (y-or-n-p "Push notes to github? ")
    (async-shell-command
     (concat "cd " org-directory
             "; git add --all && git commit -m 'emacs timer: "
             (format-time-string "[%F %a %T]'")
             "; git push")))
  (setq xr/auto-timer
        (run-with-timer 3600 nil #'xr/auto-session)))

(defun xr/disable-auto-session ()
  "Disalbe auto session."
  (interactive)
  (when (timerp xr/auto-timer)
    (setq xr/auto-timer (cancel-timer xr/auto-timer))))


(add-hook #'after-init-hook
          (lambda ()
            (async-shell-command "~/bin/hs -d ~/Downloads" "*hs-daemon*")))

(defun xr/change-hs-root (path)
  (interactive)
  (let ((url-request-method "PUT"))
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://localhost"
                 (expand-file-name path)))
      (buffer-string))))

(defun xr/change-hs-on-dired ()
  (interactive)
  (xr/change-hs-root dired-directory))

(defun xr/ifconfig ()
  (interactive)
  (message (format-network-address (car (network-interface-info "en0")) t)))


(provide 'init-xr)
;;; init-xr.el ends here
