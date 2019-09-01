;;; Compiled snippets and support files for `fundamental-mode'
;;; contents of the .yas-setup.el support file:
;;;
;; -*- no-byte-compile: t; -*-

(eval-and-compile
  (require 'cl-lib)
  (require 'yasnippet)
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'if-let* #'if-let)
      (defalias 'when-let* #'when-let))))

(defun %alias (uuid &optional mode)
  "Expand a snippet with UUID in MODE."
  (if-let* ((snippet (let ((yas-choose-tables-first nil) ; avoid prompts
                           (yas-choose-keys-first nil))
                       (cl-loop for tpl in (yas--all-templates
                                            (yas--get-snippet-tables mode))
                                if (string= uuid (yas--template-uuid tpl))
                                return tpl))))
      (yas-expand-snippet snippet)
    (error "Couldn't find snippet" &optional ARGS)))

(defun %t ()
  "Insert an inactive timestamp."
  (format-time-string "[%F %a %R]" (current-time)))
;;; Snippet definitions:
;;;
(yas-define-snippets 'fundamental-mode
                     '(("t" "`(%t)`" "inactive timestamp" nil nil nil "/Users/kevin/.config/doom/snippets/fundamental-mode/time" nil nil)))


;;; Do not edit! File generated at Sat Jul  6 16:51:36 2019
