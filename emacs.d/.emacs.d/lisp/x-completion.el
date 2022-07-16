;;; x-completion.el --- x-completion -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; corfu
(require 'corfu)
(setq corfu-auto t)
(setq corfu-auto-prefix 3)
(setq corfu-max-width 60)
(setq corfu-quit-no-match t)
(setq tab-always-indent 'complete)

(define-key corfu-map " " #'corfu-insert-separator)
(define-key corfu-map [tab] #'x/tab)
(define-key corfu-map [(shift tab)] #'corfu-previous)
(define-key corfu-map [return] #'corfu-insert)
(define-key corfu-map [escape] #'corfu-quit)

(global-corfu-mode)

;;; icon
(require 'kind-icon)

(setq kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;;; corfu-doc
(require 'corfu-doc)

(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)

;;; cape
(setq cape-dabbrev-min-length 2)

(add-to-list 'completion-at-point-functions #'cape-file)
;; (add-to-list 'completion-at-point-functions #'cape-tex)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

(defun x-completion--disable-auto-locally ()
  "Disable auto completing locally."
  (setq-local corfu-auto nil)
  (corfu-mode))

;;; eshell
(add-hook 'eshell-mode-hook #'x-completion--disable-auto-locally)

;;; org
(with-eval-after-load 'org
  (defun x-completion--org-setup ()
    (x-completion--disable-auto-locally)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  (add-hook 'org-mode-hook #'x-completion--org-setup))

;;; minibuffer
(defun x-completion--enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'x-completion--enable-in-minibuffer)

;;; copilot
(add-hook 'prog-mode-hook #'copilot-mode)

(defun x/tab ()
  (interactive)
  (or (copilot-accept-completion)
      (not (= (corfu-next) -1))
      (indent-for-tab-command)))

(global-set-key (kbd "TAB") #'x/tab)

(provide 'x-completion)
;;; x-completion.el ends here
