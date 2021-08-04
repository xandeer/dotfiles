;;; init-folding.el --- Support code and region folding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf hideshow
  :after hydra
  :hook
  ((emacs-lisp-mode-hook
    js-mode-hook
    typescript-mode-hook
    kotlin-mode-hook
    cider-mode-hook)
   . hs-minor-mode)
  :bind*
  ("C-c ,"   . hydra-hs/body)
  :hydra
  (hydra-hs (:hint nil)
            "
Hide^^            ^Show^            ^Toggle^    ^Navigation^
----------------------------------------------------------------
_h_ hide all      _s_ show all      _t_oggle    _n_ext line
_d_ hide block    _a_ show block              _p_revious line
_l_ hide level

_SPC_ cancel
"
    ("s" hs-show-all)
    ("h" hs-hide-all)
    ("a" hs-show-block)
    ("d" hs-hide-block)
    ("t" hs-toggle-hiding)
    ("l" hs-hide-level)
    ("n" forward-line)
    ("p" (forward-line -1))
    ("SPC" nil)))

(provide 'init-folding)
;;; init-folding.el ends here
