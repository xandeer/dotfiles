;;; x-racket.el --- racket -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use 'racket-mode)

(with-eval-after-load 'racket-mode
  (add-hook 'racket-mode-hook #'racket-xp-mode)

  (x/define-keys
   racket-mode-map
   '(("TAB" x/tab))))

(with-eval-after-load 'racket-xp
  (x/define-keys
   racket-xp-mode-map
   '(("C-c d" racket-xp-describe))))

(provide 'x-racket)
;;; x-racket.el ends here
