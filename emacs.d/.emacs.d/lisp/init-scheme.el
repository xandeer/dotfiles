;;; init-scheme.el --- scheme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'geiser)
(leaf geiser
  :config
  (setq geiser-active-implementations '(guile)))

(provide 'init-scheme)
;;; init-scheme.el ends here
