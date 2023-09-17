;;; x-notes.el --- notes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/boox-note-trim ()
  "Remove the information I don't care about in the boox notes."
  (interactive)
  (let ((rxs '("^时间: 20[0-9][0-9]-[0-9][0-9]-[0-9][0-9] [0-9][0-9]:[0-9][0-9]\n"
               "^【原文】"
               "^【批注】\n"
               "^-------------------\n")))
    (seq-do (lambda (regexp)
              (x/replace regexp ""))
            rxs)))

(defun x/escape-readwise ()
  "Convert Notes to comments, and flush double newlines."
  (interactive)
  (save-excursion
    (replace-regexp "\n[*]\\{2\\}Note:[*]\\{2\\} \\(.*\\)$" "#+begin_comments\n\\1\n#+end_comments"))
  (x/flush-double-newlines))

(provide 'x-notes)
;;; x-notes.el ends here
