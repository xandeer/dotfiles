;;; x-scheme.el --- scheme -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf geiser
  :disabled t
  :straight t
  :config
  (setq geiser-active-implementations '(guile)))

(provide 'x-scheme)
;;; x-scheme.el ends here
