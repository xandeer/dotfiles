;;; x-window.el --- x-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun x/delete-window-or-frame ()
  (interactive)
  (if (eq (window-deletable-p) 't)
      (delete-window)
    (delete-frame)))

;;;###autoload
(defun x/roam-or-projectile-find-file (&optional window)
  (when window
    (select-window window))
  (if (x/roam-buffer-p)
      (org-roam-node-find)
    (projectile-find-file)))

;;;###autoload
(defun x/split-below-find-file ()
  "Split below and find file."
  (interactive)
  (x/roam-or-projectile-find-file (split-window-below)))

;;;###autoload
(defun x/split-right-find-file ()
  "Split right and find file."
  (interactive)
  (x/roam-or-projectile-find-file (split-window-right)))

(require-package 'ace-window)

(global-set-key (kbd "H-o") 'ace-window)
(global-set-key (kbd "H-0") 'x/delete-window-or-frame)
(global-set-key (kbd "H-1") 'delete-other-windows)
(global-set-key (kbd "H-2") 'x/split-below-find-file)
(global-set-key (kbd "H-3") 'x/split-right-find-file)
(global-set-key [remap split-window-right] 'x/split-right-find-file)
(global-set-key [remap split-window-below] 'x/split-below-find-file)
(global-set-key (kbd "C-x x") 'ace-swap-window)

(with-eval-after-load 'ace-window
  (setq aw-dispatch-always nil))

(provide 'x-window)
;;; x-window.el ends here