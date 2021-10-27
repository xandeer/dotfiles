;;; init-link-hint.el --- init-link-hint -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf link-hint
  :straight t
  :require t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))

(provide 'init-link-hint)
;;; init-link-hint.el ends here
