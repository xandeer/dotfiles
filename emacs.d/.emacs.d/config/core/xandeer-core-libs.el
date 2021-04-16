;;; xandeer-core-libs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Configuration Core Libs.

;;; Code:

;;;###autoload
(defun xandeer-enlist (exp)
 "Return EXP wrapped in a list, or as-is if already a list."
 (declare (pure t) (side-effect-free t))
 (if (listp exp) exp (list exp)))

(defun xandeer/reload-init ()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(defun xandeer/eval-buffer ()
  "Evaluate the current buffer as Elisp code."
  (interactive)
  (message "Evaluating %s..." (buffer-name))
  (straight-transaction
    (if (null buffer-file-name)
        (eval-buffer)
      (when (string= buffer-file-name user-init-file)
        (straight-mark-transaction-as-init))
      (load-file buffer-file-name)))
  (message "Evaluating %s... done." (buffer-name)))

;;;###autoload
(defun xandeer/insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%b %d, %Y')")))

;;;###autoload
(defun xandeer/insert-current-filename ()
  "Insert current buffer filename."
  (interactive)
  (insert (file-relative-name buffer-file-name)))

;;;###autoload
(defun posframe-poshandler-frame-top-center (info)
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (round (* 0.02 (x-display-pixel-height)))))

;;;###autoload
(if (fboundp 'with-eval-after-load)
    (defalias 'after-x 'with-eval-after-load)
  (defmacro after-x (feature &rest body)
    "Eval BODY afetr FEATURE have loaded."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(provide 'xandeer-core-libs)
;;; xandeer-core-libs.el ends here
