;;; xandeer-core-libs.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Configuration Core Libs.

;;; Code:

(straight-use-package 'switch-window)

(leaf switch-window
  :doc "Offer a *visual* way to choose a window to switch to."
  :url "https://github.com/dimitri/switch-window"
  :bind
  ("C-x o" . switch-window)
  ("C-x 1" . toggle-delete-other-windows)
  ("C-x |" . split-window-horizontally-instead)
  ("C-x _" . split-window-vertically-instead)
  ("C-x x" . xandeer/split-window)
  ("C-x 2" . xandeer/split-below-find-file)
  ("C-x 3" . xandeer/split-right-find-file)
  :custom
  (switch-window-shortcut-style . 'alphabet)
  (switch-window-timeout        . nil)
  :config
  (defun xandeer/roam-or-projectile-find-file (&optional count file)
    (if (xandeer/is-roam-buffer)
        (org-roam-find-file)
      (counsel-projectile-find-file)))

  (defun xandeer/split-below-find-file ()
    "Split below and find file"
    (interactive)
    (split-window-below)
    (xandeer/roam-or-projectile-find-file))

    (defun xandeer/split-right-find-file ()
      "Split right and find file"
      (interactive)
      (split-window-right)
      (xandeer/roam-or-projectile-find-file))

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
    (defun xandeer/split-window()
      "Split the window to see the most recent buffer in the other window.
  Call a second time to restore the original window configuration."
      (interactive)
      (if (eq last-command 'xandeer-split-window)
          (progn
            (jump-to-register :xandeer-split-window)
            (setq this-command 'xandeer-unsplit-window))
        (window-configuration-to-register :xandeer/split-window)
        (switch-to-buffer-other-window nil))))

(provide 'xandeer-core-window)
;;; xandeer-core-window.el ends here
