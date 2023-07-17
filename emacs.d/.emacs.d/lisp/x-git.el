;;; x-git.el --- x-git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; diff-hl
;; (autoload #'diff-hl-dired-mode "diff-hl-dired" nil t)
;; (add-hook 'dired-mode-hook #'diff-hl-dired-mode)

(autoload #'magit-stage "magit" nil t)
;;; magit
(with-eval-after-load 'magit
  (setq magit-save-repository-buffers 'dontask)
  (setq magit-commit-ask-to-stage 'verbose)
  (setq magit-diff-refine-hunk t)
  ;; (autoload #'diff-hl-magit-post-refresh "diff-hl" nil t)
  ;; (autoload #'diff-hl-magit-pre-refresh "diff-hl" nil t)
  ;; (autoload #'no-trailing-whitespace "magit" nil t)
  ;; (add-hook 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
  ;; (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
  ;; (add-hook 'magit-post-refresh-hook #'no-trailing-whitespace)
  ;; (abridge-diff-mode)

  (x/define-keys ctl-x-map
                 '(("g" magit-status)
                   ("M-g" magit-dispatch)) )
  (x/define-keys magit-status-mode-map
                 '(("q" kill-current-buffer)
                   ("K" magit-discard)))

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

(defun x-magit--open (path)
  "Open the Magit status buffer for the Git repository at PATH.
The function expands the given PATH to an absolute path."
  (interactive)
  (magit-status (expand-file-name path)))

(transient-define-prefix x/transient-magit-status ()
  "Transient for Magit status."
  [["Magit"
    ;; ("c" "Exercism/clojure" (lambda () (interactive) (magit-status (expand-file-name "~/Exercism/clojure/"))))
    ("d" "Dotfiles" (lambda () (interactive) (magit-status (expand-file-name "~/projects/personal/dotfiles"))))
    ;; ("e" "Exercism/elixir" (lambda () (interactive) (magit-status (expand-file-name "~/Exercism/elixir/"))))
    ("h" "Heart-music" (lambda () (interactive) (magit-status (expand-file-name "~/projects/personal/heart-music/"))))
    ;; ("k" "Exercism/kotlin" (lambda () (interactive) (magit-status (expand-file-name "~/Exercism/kotlin/"))))
    ;; ("l" "Android-lab" (lambda () (interactive) (magit-status (expand-file-name "~/projects/personal/android-lab/"))))
    ("n" "Notes" (lambda () (interactive) (magit-status (expand-file-name org-directory))))
    ;; ("t" "Exercism/typescript" (lambda () (interactive) (magit-status (expand-file-name "~/Exercism/typescript/"))))
    ("w" "Work" (lambda () (interactive) (magit-status (expand-file-name x/work-directory))))]])

(global-set-key (kbd "H-m") #'x/transient-magit-status)

;;; vc key bindings
(x/define-keys
 vc-prefix-map
 '(("s" x/stage-hunk)
   ("S" magit-stage)
   ("C-l" git-link)
   ("r" git-gutter:revert-hunk)
   ("p" git-gutter:popup-hunk)
   ("j" git-gutter:next-hunk)
   ("k" git-gutter:previous-hunk)))

;;; git-messenger
(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t)
  (define-key vc-prefix-map (kbd "p") #'git-messenger:popup-message))

;;; git link
(setq git-link-open-in-browser t)

;;; forge
;; (with-eval-after-load 'magit
;;   (require 'forge))

;;; gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode t)

;; standardize default fringe width
(fringe-mode '4)
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

(defun x/stage-hunk ()
  "Stage current hunk without ask."
  (interactive)
  (let ((git-gutter:ask-p nil))
    (git-gutter:stage-hunk)))

(with-eval-after-load 'flycheck
  ;; let diff have left fringe, flycheck can have right fringe
  (setq flycheck-indication-mode 'right-fringe)
  ;; A non-descript, left-pointing arrow
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

;;; utils
(defun x--git-clone (project directory)
  "Clone a git PROJECT into a given DIRECTORY."
  (x/start-process (format "git -C %s clone %s " directory project) t))

(defcustom x/personal-projects-directory (expand-file-name "~/projects/personal")
  "The directory where I keep my personal projects."
  :type 'directory)

(defcustom x/others-projects-directory (expand-file-name "~/projects/others")
  "The directory where I keep others projects."
  :type 'directory)

(defun x/git-clone-personal (project)
  "Clone a git PROJECT into `x/personal-projects-directory'."
  (interactive "sProject: ")
  (x--git-clone project x/personal-projects-directory))

(defun x/git-clone-others (project)
  "Clone a git PROJECT into `x/others-projects-directory'."
  (interactive "sProject: ")
  (x--git-clone project x/others-projects-directory))

(provide 'x-git)
;;; x-git.el ends here
