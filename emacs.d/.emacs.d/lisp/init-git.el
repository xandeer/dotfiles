;;; init-git.el --- init-git -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf abridge-diff
  :doc "Emacs package for refining diff hunks with very long lines (as in LaTeX files)."
  :url "https://github.com/jdtsmith/abridge-diff"
  :tag "diffs" "magit" "tools"
  :after magit
  :init (abridge-diff-mode 1))

(leaf diff-hl
  :doc "Emacs package for highlighting uncommitted changes"
  :url "https://github.com/dgutov/diff-hl"
  :tag "vc" "diff"
  :hook ((dired-mode-hook         . diff-hl-dired-mode)
         (magit-pre-refresh-hook  . diff-hl-magit-pre-refresh)
         (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(leaf magit
  :doc "It's Magit! A Git porcelain inside Emacs."
  :url "https://github.com/magit/magit"
  :tag "git" "tools" "vc"
  :commands magit-status
  :hook ((magit-popup-mode-hook . no-trailing-whitespace))
  :custom ((magit-diff-refine-hunk . t)
           (vc-handled-backends    . nil))
  :bind (("C-x g"      . magit-status)
         ("C-x M-g"    . magit-dispatch-popup)
         (:magit-status-mode-map
          ("C-M-<up>"  . magit-section-up))
         (:vc-prefix-map
          ("f"         . vc-git-grep))))

(leaf git-messenger
  :doc "git-messenger.el provides function that popup commit message at current line."
  :url "https://github.com/emacsorphanage/git-messenger"
  :tag "convenience" "vc"
  :custom (git-messenger:show-detail . t)
  :bind (:vc-prefix-map
         ("p" . git-messenger:popup-message)))


(provide 'init-git)
;;; init-git.el ends here
