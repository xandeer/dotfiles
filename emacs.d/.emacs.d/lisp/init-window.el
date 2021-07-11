;;; init-window.el --- init-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf switch-window
  :straight t
  :doc "Offer a *visual* way to choose a window to switch to."
  :url "https://github.com/dimitri/switch-window"
  :bind
  ("C-x x" . window-swap-states)
  ("H-o"   . switch-window)
  ("H-1"   . delete-other-windows)
  ("H-2"   . split-window-below)
  ("H-3"   . split-window-right)
  ("H-w"   . xr/delete-window-or-frame)
  :custom
  (switch-window-shortcut-style . 'alphabet)
  (switch-window-timeout        . nil)
  :advice
  (:filter-return split-window-below xr/roam-or-projectile-find-file)
  (:filter-return split-window-right xr/roam-or-projectile-find-file)
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

(provide 'init-window)
;;; init-window.el ends here
