;;; init-scheme.el --- scheme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf geiser
  :straight t
  :config
  (setq geiser-active-implementations '(guile)))

(provide 'init-scheme)
;;; init-scheme.el ends here
