;;; init-window.el --- init-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf switch-window
  :straight t
  :doc "Offer a *visual* way to choose a window to switch to."
  :url "https://github.com/dimitri/switch-window"
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . toggle-delete-other-windows)
  ("C-x |" . split-window-horizontally-instead)
  ("C-x _" . split-window-vertically-instead)
  ("C-x x" . window-swap-states)
  ("C-x 2" . xr/split-below-find-file)
  ("C-x 3" . xr/split-right-find-file)
  :custom
  (switch-window-shortcut-style . 'alphabet)
  (switch-window-timeout        . nil)
  :config
  (defun xr/roam-or-projectile-find-file (&optional count file)
    (if (xr/is-roam-buffer)
        (xr/roam-find-file)
      (counsel-projectile-find-file)))

  (defun xr/split-below-find-file ()
    "Split below and find file"
    (interactive)
    (split-window-below)
    (xr/roam-or-projectile-find-file))

  (defun xr/split-right-find-file ()
    "Split right and find file"
    (interactive)
    (split-window-right)
    (xr/roam-or-projectile-find-file))

  (defun toggle-delete-other-windows ()
    "Delete other windows in frame if any, or restore previous window config."
    (interactive)
    (if (and winner-mode
             (equal (selected-window) (next-window)))
        (winner-undo)
      (delete-other-windows)))

  (defun split-window-horizontally-instead ()
    "Kill any other windows and re-split such that the current window is on the top half of the frame."
    (interactive)
    (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
      (delete-other-windows)
      (split-window-horizontally)
      (when other-buffer
        (set-window-buffer (next-window) other-buffer))))

  (defun split-window-vertically-instead ()
    "Kill any other windows and re-split such that the current window is on the left half of the frame."
    (interactive)
    (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
      (delete-other-windows)
      (split-window-vertically)
      (when other-buffer
        (set-window-buffer (next-window) other-buffer))))

  ;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
  (defun xr/split-window()
    "Split the window to see the most recent buffer in the other window.
    Call a second time to restore the original window configuration."
    (interactive)
    (if (eq last-command 'xandeer-split-window)
        (progn
          (jump-to-register :xandeer-split-window)
          (setq this-command 'xandeer-unsplit-window))
      (window-configuration-to-register :xr/split-window)
      (switch-to-buffer-other-window nil))))


(provide 'init-window)
;;; init-window.el ends here
