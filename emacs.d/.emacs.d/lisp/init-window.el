;;; init-window.el --- init-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun xr/delete-window-or-frame ()
  (interactive)
  (if (eq (window-deletable-p) 't)
      (delete-window)
    (delete-frame)))

(defun xr/roam-or-projectile-find-file (&optional window)
  (when window
    (select-window window))
  (if (xr/roam-buffer-p)
      (org-roam-node-find)
    (projectile-find-file)))

(defun xr/split-below-find-file ()
  "Split below and find file."
  (interactive)
  (xr/roam-or-projectile-find-file (split-window-below)))

(defun xr/split-right-find-file ()
  "Split right and find file."
  (interactive)
  (xr/roam-or-projectile-find-file (split-window-right)))

(require-package 'ace-window)
(require 'ace-window)

(with-eval-after-load 'ace-window
  (setq aw-dispatch-always nil)

  (global-set-key (kbd "H-o") 'ace-window)
  (global-set-key (kbd "H-0") 'xr/delete-window-or-frame)
  (global-set-key (kbd "H-1") 'delete-other-windows)
  (global-set-key (kbd "H-2") 'xr/split-below-find-file)
  (global-set-key (kbd "H-3") 'xr/split-right-find-file)
  (global-set-key [remap split-window-right] 'xr/split-right-find-file)
  (global-set-key [remap split-window-below] 'xr/split-below-find-file)
  (global-set-key (kbd "C-x x") 'ace-swap-window))

(provide 'init-window)
;;; init-window.el ends here
