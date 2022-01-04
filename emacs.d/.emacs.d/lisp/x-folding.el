;;; x-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defhydra x-hydra-hideshow (:exit t :columns 4 :idle 0.3)
  "
Hideshow\n"
  ("H-s" hs-show-all "show all")
  ("H-h" hs-hide-all "hide all")
  ("s" hs-show-block "show block")
  ("h" hs-hide-block "hide block")
  ("k" hs-toggle-hiding "toggle")
  ("l" hs-hide-level "hide level"))
(global-set-key (kbd "H-h") #'x-hydra-hideshow/body)

(mapcar (lambda (hook) (add-hook hook #'hs-minor-mode))
        '(emacs-lisp-mode-hook
          js-mode-hook
          typescript-mode-hook
          kotlin-mode-hook
          cider-mode-hook))

(with-eval-after-load 'hideshow
  (add-to-list 'hs-special-modes-alist '(kotlin-mode "{" "}" "/[*/]" nil nil)))
(provide 'x-folding)
;;; x-folding.el ends here
