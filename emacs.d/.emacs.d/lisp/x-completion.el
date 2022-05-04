;;; x-completion.el --- x-completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(cape :host github
        :repo "minad/cape"
        :branch "main"))
;; required by kind-icon
(straight-register-package
 '(svg-lib :host github
           :repo "rougier/svg-lib"
           :branch "master"))
(straight-register-package
 '(kind-icon :host github
        :repo "jdtsmith/kind-icon"
        :branch "main"))
(straight-register-package
 '(corfu-doc :host github
        :repo "galeo/corfu-doc"
        :branch "main"))

(require-package 'corfu)
(require-package 'cape)
(require-package 'kind-icon t)
(require-package 'corfu-doc)

(setq corfu-auto t)
(setq corfu-quit-no-match t)
(setq corfu-auto-prefix 3)
(corfu-global-mode)

(setq tab-always-indent 'complete)
(setq cape-dabbrev-min-length 2)

(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(add-hook 'corfu-mode-hook #'corfu-doc-mode)
;; (remove-hook 'corfu-mode-hook #'corfu-doc-mode)

(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
(define-key corfu-map " " #'corfu-insert-separator)
(define-key corfu-map [tab] #'corfu-next)
(define-key corfu-map [(shift tab)] #'corfu-previous)
(define-key corfu-map [return] #'corfu-insert)
(define-key corfu-map [escape] #'corfu-quit)

(add-to-list 'completion-at-point-functions #'cape-file)
;; (add-to-list 'completion-at-point-functions #'cape-tex)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

(defun x-completion--disable-auto-locally ()
  "Disable auto completing locally."
  (setq-local corfu-auto nil)
  (corfu-mode))

(with-eval-after-load 'org
  (defun x--cape-org-setup ()
    (x-completion--disable-auto-locally)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  (add-hook 'org-mode-hook #'x--cape-org-setup))

(add-hook 'eshell-mode-hook #'x-completion--disable-auto-locally)

(defun x-completion--enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'x-completion--enable-in-minibuffer)

(provide 'x-completion)
;;; x-completion.el ends here
