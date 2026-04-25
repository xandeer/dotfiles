;;; x-org-attach.el --- org attach -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-attach-use-inheritance t)
(setq org-attach-auto-tag nil)
(setq org-attach-id-dir (x/expand-note ".attach/"))
(setq org-attach-store-link-p 'attached)

(with-eval-after-load 'org
  (require 'org-attach))

(with-eval-after-load 'org-attach
  (defun x/org-set-attach-dir ()
    "Set the attachment directory for the current Org buffer."
    (save-excursion
      (goto-char (point-min))
      (let* ((org-id (org-id-get-create))
             (attach-dir (concat (unless (equal default-directory (expand-file-name org-directory))
                                   "../")
                                 ".attach/"
                                 (org-attach-id-uuid-folder-format org-id))))
        (org-entry-put (point) "DIR" attach-dir))))

  (defun x/org-attach-attach (file &optional visit-dir method)
    "Move/copy/link FILE into the attachment directory of the current outline node.
If VISIT-DIR is non-nil, visit the directory with dired.
METHOD may be `cp', `mv', `ln', `lns' or `url' default taken from
`org-attach-method'."
    (interactive
     (list
      (read-file-name "File to keep as an attachment: "
                      (or (progn
                            (require 'dired-aux)
                            (dired-dwim-target-directory))
                          (expand-file-name "~/Downloads/")))
      current-prefix-arg
      nil))
    (x/org-set-attach-dir)
    (setq method (or method org-attach-method))
    (let ((basename (file-name-nondirectory file)))
      (let* ((attach-dir (org-attach-dir 'get-create))
             (attach-file (expand-file-name basename attach-dir)))
        (cond
         ((eq method 'mv) (rename-file file attach-file))
         ((eq method 'cp) (copy-file file attach-file))
         ((eq method 'ln) (add-name-to-file file attach-file))
         ((eq method 'lns) (make-symbolic-link file attach-file))
         ((eq method 'url) (url-copy-file file attach-file)))
        (run-hook-with-args 'org-attach-after-change-hook attach-dir)
        (org-attach-tag)
        (cond ((eq org-attach-store-link-p 'attached)
               (push (list (concat "attachment:" (file-name-nondirectory attach-file))
                           (file-name-nondirectory attach-file))
                     org-stored-links))
              ((eq org-attach-store-link-p t)
               (push (list (concat "file:" file)
                           (file-name-nondirectory file))
                     org-stored-links))
              ((eq org-attach-store-link-p 'file)
               (push (list (concat "file:" attach-file)
                           (file-name-nondirectory attach-file))
                     org-stored-links)))
        (if visit-dir
            (dired attach-dir)
          (message "File %S is now an attachment" basename)))))
  (advice-add 'org-attach-attach :override #'x/org-attach-attach)

  (defun x/copy-org-attachments-to-downloads ()
    "Copy org file attachments to the '~/Downloads' directory."
    (interactive)
    (let ((attachments-dir (org-attach-dir))
          (downloads-dir (expand-file-name "~/Downloads")))
      (if (and attachments-dir (file-directory-p attachments-dir))
          (progn
            (dolist (file (directory-files attachments-dir t))
              (unless (file-directory-p file)
                (let ((new-file (expand-file-name (file-name-nondirectory file) downloads-dir)))
                  (copy-file file new-file t))))
            (message "Attachments copied to '~/Downloads' directory."))
        (message "No attachments found."))))

  (defun x/delete-attach-and-properties-block ()
    "Delete 'ATTACH:' from the heading and the :PROPERTIES: block in the current subtree."
    (interactive)
    (save-excursion
      (org-back-to-heading t)
      (let ((start (point))
            (end (org-end-of-subtree)))
        (goto-char start)
        (when (re-search-forward "\\s-*ATTACH:" (line-end-position) t)
          (replace-match ""))
        (goto-char start)
        (when (re-search-forward "^\\s-*:PROPERTIES:" end t)
          (beginning-of-line)
          (let ((properties-start (point)))
            (when (re-search-forward "^\\s-*:END:" end t)
              (forward-line)
              (delete-region properties-start (point))))))))

  (defun x/org-reattach ()
    "Reattach files for the current Org node."
    (interactive)
    (x/copy-org-attachments-to-downloads)
    (x/delete-attach-and-properties-block)
    (funcall-interactively #'org-attach)))

(provide 'x-org-attach)
;;; x-org-attach.el ends here
