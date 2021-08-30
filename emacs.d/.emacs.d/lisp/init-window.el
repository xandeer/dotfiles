;;; init-window.el --- init-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf switch-window
  :straight t
  :after hydra
  :bind
  ("C-x x"                    . window-swap-states)
  ("H-o"                      . switch-window)
  ("H-1"                      . delete-other-windows)
  ("H-2"                      . xr/split-below-find-file)
  ("H-3"                      . xr/split-right-find-file)
  ("H-w"                      . hydra-window/body)
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
        (org-roam-node-find)
      (counsel-projectile-find-file)))

  (defun xr/split-below-find-file ()
    "Split below and find file"
    (interactive)
    (xr/roam-or-projectile-find-file (split-window-below)))

  (defun xr/split-right-find-file ()
    "Split right and find file"
    (interactive)
    (xr/roam-or-projectile-find-file (split-window-right)))

  (defun xr/split-up-find-file ()
    "Split up and find file"
    (interactive)
    (split-window-vertically)
    (xr/roam-or-projectile-find-file))

  (defun xr/split-left-find-file ()
    "Split left and find file"
    (interactive)
    (split-window-horizontally)
    (xr/roam-or-projectile-find-file))
  :hydra
  (hydra-window
   (:color red :hint nil)
   "
Movement^^    ^Split^       ^Switch^               ^Delete^
----------------------------------------------------------------
_h_ ←       	_H_ ←         _b_uffer               _dw_indow
_j_ ↓         _J_ ↓         _B_uffer other window  _db_uffer
_k_ ↑         _K_ ↑                                _df_rame
_l_ →         _L_ →                                _o_nly this
----------------------------------------------------------------
  Roam: _r_oam find		_R_oam random

Cancel: _q_ cancel
"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("H" xr/split-left-find-file)
   ("J" xr/split-below-find-file)
   ("K" xr/split-up-find-file)
   ("L" xr/split-right-find-file)

   ("o" delete-other-windows :exit t)
   ("dw" delete-window)
   ("db" kill-buffer)
   ("df" delete-frame :exit t)
   ("f" make-frame :exit t)
   ("=" balance-windows)
   ("r" org-roam-node-find)
   ("R" org-roam-node-random)
   ("b" ivy-switch-buffer)
   ("B" counsel-switch-buffer-other-window)

   ("q" nil)))

(provide 'init-window)
;;; init-window.el ends here
