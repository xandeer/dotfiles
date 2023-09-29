;;; x-buffer.el --- buffer utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (proper-list-p exp) exp (list exp)))

(defun doom-buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODES.

If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (doom-enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (apply #'provided-mode-derived-p
                                   (buffer-local-value 'major-mode buf)
                                   modes))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (buffer-list)))))

(x/define-keys global-map
               '(("M-k" previous-buffer)
                 ("M-[" previous-buffer)
                 ("M-]" next-buffer)))

(x/define-keys ctl-x-map
               '(("k" previous-buffer)
                 ;; ("k" kill-current-buffer)
                 ))

;;; autosave
(require 'auto-save)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)
(setq auto-save-idle 0.5)
(add-hook 'org-capture-mode-hook #'auto-save-disable)
(add-hook 'org-capture-prepare-finalize-hook #'auto-save-enable)
(x/append-init-hook #'auto-save-enable)

;;; uniquify
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'x-buffer)
;;; x-buffer.el ends here
