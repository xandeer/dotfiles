;;; init.el --- Xandeer's Emacs Configuration file. -*- lexical-binding: t; -*-
;;; Commentary:
;; 1. package name starts with "x"
;; 2. global symbol starts with "x/"
;; 3. local symbol starts with "x--"

;;; Refs:
;; 1. https://github.com/purcell/emacs.d/

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)
(setq-default lexical-binding t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;; Adjust garbage collection thresholds during startup, and thereafter
(let ((init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (require-package 'gcmh)
              (gcmh-mode)
              ;; (setq gcmh-verbose t)
              (setq gcmh-low-cons-threshold #x800000)
              (setq gcmh-high-cons-threshold #x880000))))

;;; Bootstrap
;; straight, leaf, hydra, no-littering
(require 'x-bootstrap)
(require 'x-init-utils)
(require 'x-utils)
;; osx
(require 'x-osx)
(require 'x-hydra)
;; set "PATH" and `exec-path`
(require 'x-exec-path)
;; which-key, keyfreq, osx-modifiers
(require 'x-keybindings)

(require 'x-basic)
(require 'x-editor)

;;; ui
(require 'x-window)
(require 'x-recentf)
(require 'x-sessions)
(require 'x-theme)
(require 'x-modeline)

;;; org-mode
(require 'x-org)
(require 'x-org-bh)
(require 'x-org-agenda)
(require 'x-org-capture)
(require 'x-org-clock)
(require 'x-org-habit)
(require 'x-org-refile)
(require 'x-org-publish)
(require 'x-org-roam)

;;; languages
(require 'x-clojure)
(require 'x-elisp)
(require 'x-json)
(require 'x-kt)
(require 'x-web)

;;; tools
(require 'x-anzu)
(require 'x-avy)
(require 'x-calendar)
(require 'x-company)
(require 'x-consult)
(require 'x-dictionary)
(require 'x-dired)
(require 'x-embark)
(require 'x-eva)
(require 'x-flycheck)
(require 'x-folding)
(require 'x-hippie-expand)
(require 'x-imenu-list)
;; (require 'x-ivy)
(require 'x-jieba)
;; (require 'x-key-chord)
(require 'x-link-hint)
(require 'x-lispy)
(require 'x-lsp)
(require 'x-git)
(require 'x-meow)
(require 'x-pass)
(require 'x-pinyin)
(require 'x-projectile)
(require 'x-rime)
(require 'x-search-engine)
(require 'x-sh)
(require 'x-skeleton)
(require 'x-telega)
(require 'x-vertico)

;;; chores
(require 'x-mouse)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(defvar x/writing-config? nil)
(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load-file local-file)))

;;; after loaded
(defun x/load-init-session ()
  (interactive)
  ;; (require 'server)
  ;; (unless (server-running-p)
  ;; (server-start))
  (org-roam-node-random)
  (eva-mode)
  (eva-set-date-today)
  (x/auto-session))

(let ((init-fn
       (if x/writing-config?
           (lambda () (find-file (expand-file-name "init.el" user-emacs-directory)))
         #'x/load-init-session)))
  (run-with-idle-timer 1 nil init-fn))

(x/append-init-hook 'toggle-frame-maximized)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
