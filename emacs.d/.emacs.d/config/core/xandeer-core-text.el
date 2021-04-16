;;; xandeer-core-text.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Core Text file.

;;; Code:

;;;###autoload
(defun xandeer/delete-backward-word (arg)
  "Like `backward-kill-word', but doesn't affect the kill-ring."
  (interactive "p")
  (let (kill-ring)
    (backward-kill-word arg)))

;;;###autoload
(defun xandeer/region-active-p ()
  "Return non-nil if selection is active."
  (declare (side-effect-free t))
  (use-region-p))

;;;###autoload
(defun xandeer/region-beginning ()
  "Return beginning position of selection."
  (declare (side-effect-free t))
  (region-beginning))

;;;###autoload
(defun xandeer/region-end ()
  "Return end position of selection."
  (declare (side-effect-free t))
  (region-end))

;;;###autoload
(defun xandeer/thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.
Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.
NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((xandeer/region-active-p)
         (buffer-substring-no-properties
          (xandeer/region-beginning)
          (xandeer/region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; A little smarter than using `symbol-at-point', though in most cases,
         ;; xref ends up using `symbol-at-point' anyway.
         (xref-backend-identifier-at-point (xref-find-backend)))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

;;;###autoload
(defalias 'default/newline #'newline)

;;;###autoload
(defun default/newline-above ()
  "Insert an indented new line before the current one."
  (interactive)
  (beginning-of-line)
  (save-excursion (newline))
  (indent-according-to-mode))

;;;###autoload
(defun default/newline-below ()
  "Insert an indented new line after the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;;###autoload
(defun default/yank-pop ()
  "Interactively select what text to insert from the kill ring."
  (interactive)
  (call-interactively
   (cond ((fboundp 'counsel-yank-pop)    #'counsel-yank-pop)
         ((fboundp 'helm-show-kill-ring) #'helm-show-kill-ring)
         ((error "No kill-ring search backend available. Enable ivy or helm!")))))

;;;###autoload
(defun default/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun default/insert-file-path (arg)
  "Insert the file name (absolute path if prefix ARG).
If `buffer-file-name' isn't set, uses `default-directory'."
  (interactive "P")
  (let ((path (or buffer-file-name default-directory)))
    (insert
     (if arg
         (abbreviate-file-name path)
       (file-name-nondirectory path)))))

;;;###autoload
(defun default/newline-indent-and-continue-comments-a ()
  "A replacement for `newline-and-indent'.
Continues comments if executed from a commented line, with special support for
languages with weak native comment continuation support (like C-family
languages)."
  (interactive)
  (if (and (sp-point-in-comment)
           comment-line-break-function)
      (funcall comment-line-break-function nil)
    (delete-horizontal-space t)
    (newline nil t)
    (indent-according-to-mode)))

(provide 'xandeer-core-text)
;;; xandeer-core-text.el ends here
