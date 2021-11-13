;;; init-link-hint.el --- init-link-hint -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'link-hint)
(with-eval-after-load 'org
  (require 'link-hint)
  (global-set-key (kbd "C-c l o") 'link-hint-open-link)
  (global-set-key (kbd "C-c l c") 'link-hint-copy-link))

(provide 'init-link-hint)
;;; init-link-hint.el ends here
