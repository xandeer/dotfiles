;;; x-git.el --- x-git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'git-blamed)
(straight-use-package 'git-modes)
(straight-use-package 'git-timemachine)

(leaf abridge-diff
  :straight t
  :after magit
  :init (abridge-diff-mode 1))

(leaf diff-hl
  :straight t
  :hook
  (dired-mode-hook         . diff-hl-dired-mode)
  (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(leaf magit
  :straight t
  :commands magit-status
  :hook
  (magit-popup-mode-hook . no-trailing-whitespace)
  :custom
  (magit-diff-refine-hunk . t)
  :bind
  ("C-x g"   . magit-status)
  ("C-x M-g" . magit-dispatch-popup)
  (:magit-status-mode-map
   ("K" . magit-discard)))

(leaf git-messenger
  :straight t
  :custom
  (git-messenger:show-detail . t)
  :bind
  (:vc-prefix-map
   ("p" . git-messenger:popup-message)))

(provide 'x-git)
;;; x-git.el ends here