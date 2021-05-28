;;; init-bootstrap.el ---  bootstrap  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default debug-on-error         nil
              message-log-max        t
              load-prefer-newer      t
              ad-redefinition-action 'accept
              gc-cons-threshold      #x8100000)

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "localhost:8010")
     ("https" . "localhost:8010")))

(setq straight-recipes-gnu-elpa-use-mirror    t
      straight-repository-branch              "develop"
      straight-vc-git-default-clone-depth     1
      straight-enable-use-package-integration nil
      straight-check-for-modifications        '(find-when-checking))

(defvar bootstrap-version)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf leaf-keywords
  :config
  (leaf-keywords-init))

(leaf benchmark-init
  :doc "This is a simple benchmark of calls to Emacs require and load functions."
  :url "https://github.com/dholm/benchmark-init-el"
  :straight t
  :hook ((after-init . benchmark-init/deactivate))
  :init (benchmark-init/activate))

(leaf gcmh
  :doc "Use GCMH --  the Garbage Collector Magic Hack -- to adjust garbage collection."
  :url "https://gitlab.com/koral/gcmh"
  :straight t
  :custom
  (gcmh-verbose             . nil)
  (gcmh-lows-cons-threshold . #x800000)
  (gcmh-high-cons-threshold . gc-cons-threshold)
  (gcmh-idle-delay          . 3600)
  :config
  (gcmh-mode))

(leaf no-littering
  :straight t
  :require t)

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
