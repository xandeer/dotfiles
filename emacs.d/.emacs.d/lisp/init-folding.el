;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf hideshow
  :hook
  ((emacs-lisp-mode-hook
    js-mode-hook
    typescript-mode-hook
    kotlin-mode-hook
    cider-mode-hook)
   . hs-minor-mode))

(provide 'init-folding)
;;; init-folding.el ends here
