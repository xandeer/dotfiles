;;; init-xr.el --- init-xr -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun xr/replace-in-buffer (old new &optional forward)
  "Replace the string OLD with string NEW.
When FORWARD, don't go to min point."
  (unless forward (setq forward nil))
  (save-excursion
    (unless forward
      (goto-char (point-min)))
    (while (re-search-forward old nil t)
      (replace-match new))))

(defun xr/convert-chinese-quotations ()
  "Convert all [“|“ ‘|’] to [ 「|」『|』] in current buffer."
  (interactive)

  (xr/replace-in-buffer "‘" "『")
  (xr/replace-in-buffer "’" "』")
  (xr/replace-in-buffer "“" "「")
  (xr/replace-in-buffer "”" "」"))

;;;###autoload
(defun xr/org-heading-beginning-p ()
  "Whether the point is at beginning of a heading."
  (and (org-at-heading-p) (= (line-beginning-position) (point))))

(defun xr/remove-links-forward ()
  "Remove links in after current point."
  (interactive)
  (xr/remove-links t))

(defun xr/remove-links-in-buffer ()
  "Remove links in the current buffer."
  (interactive)
  (xr/remove-links nil))

(defun xr/remove-links (forward)
  "Remove links after current point.
When FORWARD, don't go to min point."
  (xr/replace-in-buffer "\\[\\[.*\\]\\[\\(.*\\)\\]\\]" "\\1" forward))

(defun xr/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name)))

(provide 'init-xr)
;;; init-xr.el ends here
