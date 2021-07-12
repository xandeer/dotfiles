;;; init-window.el --- init-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf switch-window
  :straight t
  :doc "Offer a *visual* way to choose a window to switch to."
  :url "https://github.com/dimitri/switch-window"
  :bind
  ("C-x x"                    . window-swap-states)
  ("H-o"                      . switch-window)
  ("H-1"                      . delete-other-windows)
  ("H-2"                      . xr/split-below-find-file)
  ("H-3"                      . xr/split-right-find-file)
  ("H-w"                      . xr/delete-window-or-frame)
  ([remap other-window]       . switch-window)
  ([remap split-window-below] . xr/split-below-find-file)
  ([remap split-window-right] . xr/split-right-find-file)
  ([remap delete-window]      . xr/delete-window-or-frame)
  :custom
  (switch-window-shortcut-style . 'alphabet)
  (switch-window-timeout        . nil)
  :config
  (defun xr/delete-window-or-frame ()
    (interactive)
    (if (eq (window-deletable-p) 't)
        (delete-window)
      (delete-frame)))

  (defun xr/roam-or-projectile-find-file (&optional window)
    (when window
      (select-window window))
    (if (xr/roam-buffer-p)
        (org-roam-find-file)
      (counsel-projectile-find-file)))

  (defun xr/split-below-find-file ()
    "Split below and find file"
    (interactive)
    (xr/roam-or-projectile-find-file (split-window-below)))

  (defun xr/split-right-find-file ()
    "Split right and find file"
    (interactive)
    (xr/roam-or-projectile-find-file (split-window-right))))

(provide 'init-window)
;;; init-window.el ends here
