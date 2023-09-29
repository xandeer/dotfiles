;;; x-autoloads.el --- generate autoloads -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst x/autoloads-files
  '("anki"
    "notes"
    "misc"))

(defconst x/autoloads-dir
  (expand-file-name "autoloads" user-emacs-directory))

(defun x/autoloads-generate ()
  "Generate autoloads."
  (interactive)
  (unless (file-exists-p x/autoloads-dir)
    (make-directory x/autoloads-dir t))

  (require 'autoload)

  (mapc (lambda (file)
          (let ((file (expand-file-name (concat "lisp/x-" file ".el") user-emacs-directory))
                (out (expand-file-name (concat "x-" file "-autoloads.el") x/autoloads-dir)))
            (when (file-exists-p file)
              (update-file-autoloads file t out))))
        x/autoloads-files))

(defun x/autoloads-load ()
  "Load autoloads."
  (let ((files (directory-files x/autoloads-dir t "x-.*-autoloads.el$")))
    (mapc #'load files)))

(unless (file-exists-p x/autoloads-dir)
  (x/autoloads-generate))

(x/autoloads-load)

(provide 'x-autoloads)
;;; x-autoloads.el ends here
