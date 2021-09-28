;;; init.el --- Xandeer's Emacs Configuration file. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default lexical-binding t)

;; Bootstrap
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-bootstrap)
(require 'init-xr)
(require 'init-exec-path)
(require 'init-keybindings)

(require 'init-basic)
(require 'init-editor)

;; ui
(require 'init-window)
(require 'init-recentf)
(require 'init-sessions)
(require 'init-theme)
(require 'init-modeline)

;; org-mode
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

;; languages
(require 'init-clojure)
(require 'init-elisp)
(require 'init-json)
(require 'init-kt)
(require 'init-scheme)
(require 'init-web)

;; tools
(require 'init-anzu)
(require 'init-avy)
(require 'init-calendar)
(require 'init-company)
(require 'init-dictionary)
(require 'init-dired)
(require 'init-engine-mode)
(require 'init-eva)
(require 'init-flycheck)
(require 'init-folding)
(require 'init-imenu-list)
(require 'init-ivy)
(require 'init-jieba)
(require 'init-lsp)
(require 'init-git)
(require 'init-meow)
(require 'init-pass)
(require 'init-projectile)
(require 'init-rime)
(require 'init-sh)
(require 'init-skeleton)
(require 'init-telega)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(unless (server-running-p)
  (server-start))

;; chores
(require 'init-mouse)

(require 'init-gc)
(toggle-frame-maximized)

(let ((local-file (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-file)
    (load-file local-file)))

;; after loaded
(run-with-idle-timer 1 nil
                     (lambda ()
                       (eva-mode)
                       (xr/auto-session)
                       (org-roam-node-random)))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
