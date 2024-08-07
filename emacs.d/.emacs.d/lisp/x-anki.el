;;; x-anki.el --- anki -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; require
;; anki-connect: 2055492159
;; inspector: 31746032
(setq org-anki-default-deck "Default")

;;;###autoload
(defun x/anki-cloze (beg end)
  "Transform the region between BEG and END into a cloze."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (delete-region beg end)
    (insert (format "{{c1::%s}}" text))))

(provide 'x-anki)
;;; x-anki.el ends here
