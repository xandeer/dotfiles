;;; init.el --- Xandeer's Emacs Configuration file. -*- lexical-binding: t; -*-
;;; Commentary:
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
(require 'xr-bootstrap)
(require 'xr-utils)
(require 'init-xr)
;; set "PATH" and `exec-path`
(require 'xr-exec-path)
;; which-key, keyfreq, osx-modifiers
(require 'xr-keybindings)

(require 'xr-basic)
(require 'xr-editor)

;;; ui
(require 'xr-window)
(require 'init-recentf)
(require 'init-sessions)
(require 'init-theme)
(require 'init-modeline)

;;; org-mode
(require 'init-org)
(require 'init-org-bh)
(require 'init-org-agenda)
(require 'init-org-capture)
(require 'init-org-clock)
(require 'init-org-habit)
(require 'init-org-refile)
(require 'init-org-journal)
(require 'init-org-publish)
(require 'init-org-roam)

;;; languages
(require 'init-clojure)
(require 'init-elisp)
(require 'init-json)
(require 'init-kt)
(require 'init-scheme)
(require 'init-web)

;;; tools
(require 'init-anzu)
(require 'init-avy)
(require 'init-calendar)
(require 'init-company)
(require 'init-consult)
(require 'init-dictionary)
(require 'init-dired)
(require 'init-embark)
(require 'init-eva)
(require 'init-flycheck)
(require 'init-folding)
(require 'init-hippie-expand)
(require 'init-imenu-list)
;; (require 'init-ivy)
(require 'init-jieba)
(require 'init-key-chord)
(require 'init-link-hint)
(require 'init-lsp)
(require 'init-git)
(require 'xr-meow)
(require 'init-pass)
(require 'init-pinyin)
(require 'init-projectile)
(require 'init-rime)
(require 'xr-search-engine)
(require 'init-sh)
(require 'xr-skeleton)
(require 'init-telega)
(require 'init-vertico)
(require 'init-hydra)

;;; chores
(require 'init-mouse)

;;; osx
(require 'xr-osx)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load-file local-file)))

;;; after loaded
;;;###autoload
(defun xr-load-init-session ()
  ;; (require 'server)
  ;; (unless (server-running-p)
  ;; (server-start))
  (org-roam-node-random)
  (eva-mode)
  (eva-set-date-today)
  (xr/auto-session))

(setq xr-writing-config nil)

(let ((init-fn
       (if xr-writing-config
           (lambda () (find-file (expand-file-name "init.el" user-emacs-directory)))
         'xr-load-init-session)))
  (run-with-idle-timer 1 nil init-fn))

(xr-append-init-hook 'toggle-frame-maximized)

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
