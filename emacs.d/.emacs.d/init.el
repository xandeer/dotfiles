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

(defvar x/doom? (boundp 'doom-version)
  "Whether use doom-emacs or not.")

(defvar x/vertico-posframe? t
  "Whether use vertico-posframe or not.")

(setq vanilla-path (expand-file-name "~/projects/personal/dotfiles/emacs.d/.emacs.d"))

(add-to-list 'load-path (expand-file-name "lisp" vanilla-path))

;;; Adjust garbage collection thresholds during startup, and thereafter
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
            (lambda ()
              (require 'gcmh)
              (gcmh-mode)
              ;; (setq gcmh-verbose t)
              (setq gcmh-low-cons-threshold #x800000)
              (setq gcmh-high-cons-threshold #x880000)))

;;; Bootstrap
;; straight
(require 'x-bootstrap)
(require 'x-packages)
(require 'no-littering)
(require 'x-init-utils)
(require 'x-utils)
;; osx
(require 'x-osx)
(require 'x-hydra)
;; set "PATH" and `exec-path`
(require 'x-exec-path)
;; which-key, keyfreq, osx-modifiers
(require 'x-keybindings)
(require 'x-navigation)

(require 'x-basic)
(require 'x-buffer)
(require 'x-editor)
(require 'x-docs)

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
(require 'x-elixir)
(require 'x-json)
(require 'x-kotlin)
(require 'x-web)

;;; tools
(require 'x-anzu)
(require 'x-avy)
(require 'x-browser)

(require 'x-calendar)
;; (require 'x-company)
(require 'x-completion)
(require 'x-consult)
(require 'x-dictionary)
(require 'x-dired)
(require 'x-embark)
(require 'x-eva)
(require 'x-eww)
(require 'x-flycheck)
(require 'x-folding)
(require 'x-hippie-expand)
;; (require 'x-ivy)
(require 'x-jieba)
;; (require 'x-key-chord)
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
(require 'x-template)
(require 'x-telega)
(require 'x-vertico)
;; (require 'x-point-mode)
(require 'x-xwidget)

;;; chores
(require 'x-image)
(require 'x-mouse)
(require 'x-wakatime)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(let ((local-file (expand-file-name "local.el" vanilla-path)))
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
  (x/start-timer-session))

(let ((init-fn
       (if x/configing?
           (lambda () (find-file (expand-file-name "init.el" vanilla-path)))
         #'x/load-init-session)))
  (run-with-idle-timer 1 nil init-fn))

(x/append-init-hook
 '(toggle-frame-maximized
   ;; x/selection-mode-global-enable
   ebuku))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
