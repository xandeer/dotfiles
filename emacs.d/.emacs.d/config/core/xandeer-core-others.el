;;; xandeer-core-others.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Core Text file.

;;; Code:

;;;###autoload
(defun xandeer/lsp-format-region-or-buffer ()
  "Format the buffer (or selection) with LSP."
  (interactive)
  (unless (bound-and-true-p lsp-mode)
    (user-error "Not in an LSP buffer"))
  (call-interactively
   (if (xandeer/region-active-p)
       #'lsp-format-region
     #'lsp-format-buffer)))

;;;###autoload
(defun xandeer/project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (xandeer/project-root dir)
       t))

;;;###autoload
(defun xandeer/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root (unless dir projectile-project-root))
        projectile-require-project-root)
    (projectile-project-root dir)))

(provide 'xandeer-core-others)
;;; xandeer-core-others.el ends here
