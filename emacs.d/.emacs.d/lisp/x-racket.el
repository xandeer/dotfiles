;;; x-racket.el --- racket -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use 'racket-mode)

(with-eval-after-load 'racket-mode
  (x/define-keys
   racket-mode-map
   '(("TAB" x/tab))))

(provide 'x-racket)
;;; x-racket.el ends here
