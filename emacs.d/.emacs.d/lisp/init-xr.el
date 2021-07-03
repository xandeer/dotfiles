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
    (goto-char beg)
    (while (re-search-forward old end t)
      ;; After replace, end will change.
      (when (match-string 1)
        (setq end (+ end (- (length (match-string 1)) (length (match-string 0))))))
      (replace-match new))))

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
  (xr/replace "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" beg end))

(defun xr/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name))
  (kill-buffer (buffer-name)))

(provide 'init-xr)
;;; init-xr.el ends here
