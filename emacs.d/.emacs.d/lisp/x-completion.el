;;; x-completion.el --- x-completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'corfu)
(straight-register-package
 '(cape :host github
        :repo "minad/cape"
        :branch "main"))
(require-package 'cape)

(setq corfu-auto t)
(setq corfu-auto-prefix 3)
(corfu-global-mode)

(setq tab-always-indent 'complete)
(setq cape-dabbrev-min-length 2)

(add-to-list 'completion-at-point-functions #'cape-file)
;; (add-to-list 'completion-at-point-functions #'cape-tex)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

(with-eval-after-load 'org
  (defun x--cape-org-setup ()
    (setq-local corfu-auto nil)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  (add-hook 'org-mode-hook #'x--cape-org-setup))

(provide 'x-completion)
;;; x-completion.el ends here
