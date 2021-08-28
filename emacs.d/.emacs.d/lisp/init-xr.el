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

  (xr/replace "‘" "『")
  (xr/replace "’" "』")
  (xr/replace "“" "「")
  (xr/replace "”" "」"))

;;;###autoload
(defun xr/org-heading-beginning-p ()
  "Whether the point is at beginning of a heading."
  (and (org-at-heading-p) (= (line-beginning-position) (point))))

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

(defun xr/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name))
  (kill-current-buffer))

(defun xr/convert-evernote ()
  "Remove timestamp and replace quotations."
  (interactive)
  (org-mark-subtree)
  (forward-line 1)
  (xr/replace " \\{4,\\}[0-9]\\{4\\}-.*:[0-9]\\{2\\}" "" (region-beginning) (region-end))
  (org-mark-subtree)
  (forward-line 1)
  (xr/replace "\n\\{1\\}" "\n\n" (region-beginning) (region-end))
  (org-mark-subtree)
  (forward-line 1)
  (xr/replace "\n\\{3,\\}" "\n\n" (region-beginning) (region-end))
  (org-mark-subtree)
  (forward-line 1)
  (reverse-region (region-beginning) (region-end))
  (xr/convert-chinese-quotations)
  (mark-whole-buffer)
  (unfill-toggle))

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
    (xr/replace "^\\(#\\+TITLE: \\).*" (format-time-string "\\1%B %m-%d" today))
    (xr/replace "^\\* .*" (format-time-string "* %B %d\n** %Y" today))
    (search-forward "** 2020")
    (org-set-tags (format-time-string ":%a:" today))
    (xr/replace "^\\*" "**" (point))))

(defun xr/insert-journal-in-year (year)
  "Insert a journal heading like: ** YEAR :Mon:."
  (interactive "sYear default[18], j[21], else[17]: ")
  (setq year (cond ((s-blank-str? year) 18)
                   ((s-equals? year "j") 21)
                   (t 17)))
  (goto-char (point-min))
  (let ((today (xr/journal-date year)))
    (if (= year 21) (search-forward (format-time-string "* %B %d" today))
      (goto-char (point-max)))
    (insert (format-time-string "\n** %Y" today))
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

(provide 'init-xr)
;;; init-xr.el ends here
