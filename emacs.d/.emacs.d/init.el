;;; init.el --- Xandeer's Emacs Configuration file. -*- lexical-binding: t; -*-
;;; Commentary:

;;; Refs:
;; 1. https://github.com/purcell/emacs.d/

;;; Code:

(setq-default lexical-binding t)

(defvar x/vertico-posframe? nil
  "Whether use vertico-posframe or not.")

(defun x/load-file-in-user-emacs-directory (file)
  "Load FILE in `user-emacs-directory'."
  (let ((path (expand-file-name file user-emacs-directory)))
    (if (file-exists-p path)
        (load-file path)
      (message "File %s not found." path))))

(x/load-file-in-user-emacs-directory "private.el")

(defun x/load-path-add (path)
  "Add PATH in `user-emacs-directory' to `load-path'."
  (add-to-list 'load-path (expand-file-name path user-emacs-directory)))

(mapc 'x/load-path-add '("lisp" "autoloads" "other"))

;;; Bootstrap
;; straight
(require 'x-bootstrap)
(require 'x-packages)
(require 'no-littering)

(require 'x-init-utils)
;; If I'm modifying the config, after restart open the init.el,
;; else run `x/load-init-session'.
(let ((init-fn
       (if x/configing?
           (lambda () (find-file (expand-file-name "init.el" user-emacs-directory)))
         #'x/load-init-session)))
  (run-with-idle-timer 1 nil init-fn))

(require 'x-utils)

(require 'x-start-process)

;;; key bindings
(require 'x-transients)
;; which-key, keyfreq, osx-modifiers
(require 'x-key-helper)
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
;; (require 'x-org-habit)
(require 'x-org-refile)
(require 'x-org-publish)
(require 'x-org-roam)

;;; languages
(require 'x-prog)
(require 'x-clojure)
(require 'x-elisp)
;; (require 'x-elixir)
(require 'x-json)
(require 'x-kotlin)
;; (require 'x-racket)
(require 'x-web)

;;; tools
(require 'x-anzu)
(require 'x-avy)
(require 'x-browser)
(require 'x-calendar)
(require 'azure-tts)
(require 'x-completion)
(require 'x-consult)
(require 'x-dictionary)
(require 'x-dired)
(require 'x-draw)
(require 'x-embark)
(require 'x-flycheck)
(require 'x-folding)
(require 'x-hippie-expand)
(require 'x-jieba)
(require 'x-lispy)
(require 'x-lsp)
(require 'x-git)
(require 'x-meow)
(require 'x-pass)
(require 'x-pinyin)
(require 'x-projectile)
(require 'x-repeat)
(require 'x-rime)
(require 'x-rss)
(require 'x-search-engine)
(require 'x-sh)
(require 'x-shr)
(require 'x-template)
(require 'x-telega)
(require 'x-vertico)
(require 'x-xwidget)

;; ai
(require 'x-chatgpt)
(require 'x-gpt-completion)
(require 'x-gpt-code)
(require 'x-gpt-git)

;; osx
(require 'x-osx)

;;; chores
(require 'x-image)
(require 'x-mouse)
(require 'x-wakatime)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(x/load-file-in-user-emacs-directory "local.el")

;;; autoloads
(require 'x-autoloads)

;;; after loaded
(defun x/load-init-session ()
  (interactive)
  (server-start)
  (org-roam-node-random)
  (x/start-timer-session))

(x/append-init-hook
 '(ebuku))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
