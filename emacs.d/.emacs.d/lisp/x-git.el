;;; x-git.el --- x-git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; package
(require-package 'git-blamed)
(require-package 'git-modes)
(require-package 'git-timemachine)
(require-package 'diff-hl)
(require-package 'abridge-diff)
(require-package 'magit)
(require-package 'git-messenger)

;;; diff-hl
(autoload #'diff-hl-dired-mode "diff-hl" nil t)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)

;;; magit
(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t)
  (autoload #'diff-hl-magit-post-refresh "diff-hl" nil t)
  (autoload #'diff-hl-magit-pre-refresh "diff-hl" nil t)
  ;; (autoload #'no-trailing-whitespace "magit" nil t)
  (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; (add-hook 'magit-post-refresh-hook #'no-trailing-whitespace)
  (abridge-diff-mode)

  (global-set-key (kbd "C-x g") #'magit-status)
  (global-set-key (kbd "C-x M-g") #'magit-dispatch-popup)
  (define-key magit-status-mode-map (kbd "K") #'magit-discard))

(defhydra x-hydra-magit-status (:exit t :columns 4 :idle 0.3)
	"
Magit\n"
	("n" (magit-status org-directory) "notes")
  ("w" (magit-status x/work-directory) "work")
  ("d" (magit-status (expand-file-name "~/projects/personal/dotfiles")) "dotfiles"))
(global-set-key (kbd "H-m") #'x-hydra-magit-status/body)

;;; git-messenger
(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t)
  (define-key vc-prefix-map (kbd "p") #'git-messenger:popup-message))

(provide 'x-git)
;;; x-git.el ends here
