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
(autoload #'diff-hl-dired-mode "diff-hl-dired" nil t)
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
  (global-set-key (kbd "C-x M-g") #'magit-dispatch)
  (define-key magit-status-mode-map (kbd "q") #'kill-current-buffer)
  (define-key magit-status-mode-map (kbd "K") #'magit-discard)

  (defun x/git-create-tag-and-update-chglog ()
    (interactive)
    (call-interactively #'magit-tag-create)
    (magit-push-tags "origin")
    (yarn-run "chglog"))

  ;; override default magit-tag
  (transient-define-prefix magit-tag ()
    "Create or delete a tag."
    :man-page "git-tag"
    ["Arguments"
     ("-f" "Force"    ("-f" "--force"))
     ("-a" "Annotate" ("-a" "--annotate"))
     ("-s" "Sign"     ("-s" "--sign"))
     (magit-tag:--local-user)]
    [["Create"
      ("t"  "tag"            magit-tag-create)
      ("r"  "release"        magit-tag-release)
      ("c"  "tag and chglog" x/git-create-tag-and-update-chglog)]
     ["Do"
      ("k"  "delete"  magit-tag-delete)
      ("p"  "prune"   magit-tag-prune)]]))

(defhydra x-hydra-magit-status (:exit t :columns 4 :idle 0.3)
  "
Magit\n"
  ("n" (magit-status org-directory) "notes")
  ("w" (magit-status x/work-directory) "work")
  ("d" (magit-status (expand-file-name "~/projects/personal/dotfiles")) "dotfiles")
  ("l" (magit-status (expand-file-name "~/projects/personal/android-lab/")) "android-lab")
  ("c" (magit-status (expand-file-name "~/Exercism/clojure/")) "exercism/clojure")
  ("k" (magit-status (expand-file-name "~/Exercism/kotlin/")) "exercism/kotlin")
  ("t" (magit-status (expand-file-name "~/Exercism/typescript/")) "exercism/typescript")
  ("e" (magit-status (expand-file-name "~/Exercism/elixir/")) "exercism/elixir"))

(global-set-key (kbd "H-m") #'x-hydra-magit-status/body)

;;; git-messenger
(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t)
  (define-key vc-prefix-map (kbd "p") #'git-messenger:popup-message))

;;;
(require-package 'forge)
(with-eval-after-load 'magit
  (require 'forge))

(provide 'x-git)
;;; x-git.el ends here
