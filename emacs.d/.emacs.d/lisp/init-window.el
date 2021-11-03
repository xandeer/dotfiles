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
    (counsel-projectile-find-file)))

(defun xr/split-below-find-file ()
  "Split below and find file."
  (interactive)
  (xr/roam-or-projectile-find-file (split-window-below)))

(defun xr/split-right-find-file ()
  "Split right and find file."
  (interactive)
  (xr/roam-or-projectile-find-file (split-window-right)))

(leaf ace-window
  :straight t
  :bind
  ("H-o"   . ace-window)
  ("H-0"   . xr/delete-window-or-frame)
  ("H-1"   . delete-other-windows)
  ("H-2"   . xr/split-below-find-file)
  ("H-3"   . xr/split-right-find-file)
  ("C-x x" . ace-swap-window)
  :custom
  (aw-dispatch-always . nil)
  :defer-config
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/
  (eval-when-compile
    (defmacro xr/embark-ace-action (fn)
      `(defun ,(intern (concat "xr/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (define-key embark-file-map     (kbd "o") (xr/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (xr/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (xr/embark-ace-action bookmark-jump))

  (eval-when-compile
    (defmacro xr/embark-split-action (fn split-type)
      `(defun ,(intern (concat "xr/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         (interactive)
         (select-window (funcall #',split-type))
         (call-interactively #',fn))))

  (define-key embark-file-map     (kbd "2") (xr/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "2") (xr/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (xr/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map     (kbd "3") (xr/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "3") (xr/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (xr/embark-split-action bookmark-jump split-window-right)))

(leaf switch-window
  :straight t
  :disabled t
  :after hydra
  :bind
  ("C-x x"                    . window-swap-states)
  ("H-o"                      . switch-window)
  ("H-0"                      . xr/delete-window-or-frame)
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
_h_ ←       	_H_ ←         _b_uffer               _w_indow
_j_ ↓         _J_ ↓         _B_uffer other window  _db_uffer
_k_ ↑         _K_ _s_ ↑                            _f_rame
_l_ →         _L_ _v_ →                            _o_nly this
----------------------------------------------------------------
  Roam: _R_oam find		_r_oam random
Others: _=_ balance windows
Cancel: _q_ cancel
"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("H" xr/split-left-find-file)
   ("J" xr/split-below-find-file)
   ("K" xr/split-up-find-file)
   ("s" xr/split-up-find-file)
   ("L" xr/split-right-find-file)
   ("v" xr/split-right-find-file)

   ("o" delete-other-windows :exit t)
   ("w" delete-window)
   ("db" kill-buffer)
   ("f" delete-frame :exit t)
   ("=" balance-windows)
   ("R" org-roam-node-find)
   ("r" org-roam-node-random)
   ("b" ivy-switch-buffer)
   ("B" counsel-switch-buffer-other-window)

   ("q" nil)))

(provide 'init-window)
;;; init-window.el ends here
