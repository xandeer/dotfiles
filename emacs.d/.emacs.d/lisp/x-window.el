;;; x-window.el --- x-window -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq window-divider-default-right-width 1)
(x/append-init-hook #'window-divider-mode)

;;; libs
(defun x/delete-window-or-frame ()
  (interactive)
  (if (eq (window-deletable-p) 't)
      (delete-window)
    (delete-frame)))

(defun x/roam-or-projectile-find-file (&optional window)
  (when window
    (select-window window))
  (if (x/roam-buffer-p)
      (org-roam-node-find)
    (projectile-find-file)))

(defun x/split-below-find-file ()
  "Split below and find file."
  (interactive)
  (x/roam-or-projectile-find-file (split-window-below)))

(defun x/split-right-find-file ()
  "Split right and find file."
  (interactive)
  (x/roam-or-projectile-find-file (split-window-right)))

;;; ace window
(require 'ace-window)

(setq aw-dispatch-always nil)
(setq aw-scope 'frame)
(setq aw-keys '(?j ?k ?l ?i ?h ?f ?d ?a))
;; (setq aw-ignore-current t)
(setq aw-ignored-buffers
      '("*Calc Trail*"
        " *LV*"
        ;; "*Org Agenda(a)*"
        ))

;;; bindings
(x/define-keys global-map
               '(("H-o" ace-window)
                 ("H-0" x/delete-window-or-frame)
                 ("H-1" delete-other-windows)
                 ("H-2" x/split-below-find-file)
                 ("H-3" x/split-right-find-file)
                 ("H-w" kill-current-buffer)
                 ([remap delete-window] x/delete-window-or-frame)
                 ([remap split-window-below] x/split-below-find-file)
                 ([remap split-window-right] x/split-right-find-file)))

(defun x/previous-window ()
  "Select the previous window."
  (interactive)
  (setq repeat-map 'other-window-repeat-map)
  (other-window -1))
(x/define-keys ctl-x-map '(("x" ace-swap-window)))

(x/define-keys other-window-repeat-map
               '(("u" x/previous-window)
                 ("i" x/previous-window)))

;; Display agenda buffers always at the left.
;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos (or "*Org Agenda(a)*"
;;                             "*Org Agenda(e)"
;;                             "*Org Agenda(w)"))
;;                (display-buffer-reuse-window display-buffer-in-side-window)
;;                (reusable-frames . visible)
;;                (side . left)
;;                (window-width . 0.33)))
;; (add-to-list 'display-buffer-alist
;;              `("\\`\\*Org Agenda(.)\\*"
;;                (display-buffer-reuse-window display-buffer-pop-up-frame)
;;                (reusable-frames . t)))

;;; golden ratio
(setq golden-ratio-auto-scale t)
(setq golden-ratio-max-width 80)
(golden-ratio-mode 1)

(add-hook 'buffer-list-update-hook #'golden-ratio)
(add-hook 'focus-in-hook           #'golden-ratio)
(add-hook 'focus-out-hook          #'golden-ratio)

(defvar x/window-maximized? nil)
(defun x/toggle-window-maximize ()
  "Disable `golden-ratio-mode', then `maximize-window' to maximize window.
Otherwise, enable `golden-ratio-mode'."
  (interactive)
  (if x/window-maximized?
      (golden-ratio-mode)
    (golden-ratio-mode -1)
    (maximize-window))
  (setq x/window-maximized? (not x/window-maximized?)))

;;; startup
(defun x/window-startup ()
  "Window startup."
  (toggle-frame-maximized)
  (switch-to-buffer "*scratch*")
  (select-window (split-window-right))
  (split-window-right))

(x/append-init-hook #'x/window-startup)

;;; frame
(x/define-keys ctl-x-map '(("C-o" other-frame)))
(defun x/other-frame-when-just-one-window (count &optional _ _)
  "Switch to other COUNT frame when there is just one window."
  (when (one-window-p) (other-frame count)))
(advice-add 'other-window :before #'x/other-frame-when-just-one-window)
(x/define-keys ctl-x-5-map '(("3" make-frame)
                             ("9" other-frame)))

;;; popper
;; (x/package-use 'popper)
;; (with-eval-after-load 'popper
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "\\*Async Shell Command\\*"
;;           help-mode
;;           helpful-mode
;;           compilation-mode)))
;; (x/append-init-hook #'popper-mode)

(provide 'x-window)
;;; x-window.el ends here
