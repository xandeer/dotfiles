;;; x-git.el --- x-git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (global-set-key (kbd "C-x s") #'magit-stage)
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

;;; forge
(with-eval-after-load 'magit
  (require 'forge))

;;; gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(with-eval-after-load 'git-gutter-fringe
  ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '4))

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)
  (add-function :after after-focus-change-function
              #'git-gutter:update-all-windows)

  (with-eval-after-load 'flycheck
    ;; let diff have left fringe, flycheck can have right fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))

(provide 'x-git)
;;; x-git.el ends here
