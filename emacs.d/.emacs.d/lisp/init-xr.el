;;; init-xr.el --- init-xr -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun xr/replace-in-buffer (old new)
  "Replace the string OLD with string NEW."
  (save-excursion
    (goto-char (point-min))
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

(provide 'init-xr)
;;; init-xr.el ends here
