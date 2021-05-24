;;; init-bootstrap.el ---  bootstrap  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default debug-on-error         t
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

(defmacro xandeer/s-u-p (&rest packages)
  "Straight use multiple PACKAGES.

Usage:

    \(xandeer/s-u-p nil t 'avy avx *eldoc-use* (avy :type git)
                 (:when nil ax3) (:when t 'axx ax2) (:when t axy 'axy2))

to

    \(progn
       (straight-use-package 'avy)
       (straight-use-package 'avx)
       (if *eldoc-use*
           (progn
             (straight-use-package *eldoc-use*)))
       (straight-use-package
        '(avy :type git))
       nil
       (progn
         (straight-use-package 'axx)
         (straight-use-package 'ax2))
       (progn
         (straight-use-package 'axy)
         (straight-use-package 'axy2)))"
  (declare (indent defun))
  `(progn
     ,@(cl-loop for package in packages
                when (and package
                         (not (eq package t)))
                collect
                (cond ((and (symbolp package)
                           (or (not (boundp package))
                              (fboundp package)))
                       `(straight-use-package ',package))
                      ((symbolp package)
                       `(when ,package
                          (straight-use-package ,package)))
                      ((consp package)
                       (let ((fst (car package))
                             (rst (cdr package)))
                         (cond ((eq :when fst)
                                `(when ,(car rst)
                                   (xandeer/s-u-p ,@(cdr rst))))
                               ((eq 'quote fst)
                                `(straight-use-package ,package))
                               (t `(straight-use-package ',package)))))
                      (t nil)))))


(xandeer/s-u-p leaf leaf-keywords)
(leaf-keywords-init)

;; Feature `straight-x' from package `straight' provides
;; experimental/unstable extensions to straight.el which are not yet
;; ready for official inclusion.
(leaf straight-x
  ;; Add an autoload for this extremely useful command.
  :commands (straight-x-fetch-all))

(defmacro xandeer/local-repo (repo)
  "Xandeer load local REPO."
  (let ((n-repo (format "xandeer-%s" (symbol-name repo))))
    `(progn
       (straight-use-package
        '(,(intern n-repo)
          :local-repo ,(expand-file-name
                        (format "config/%s" (symbol-name repo))
                        user-emacs-directory))))))

(straight-use-package 'benchmark-init)
(leaf benchmark-init
  :doc "This is a simple benchmark of calls to Emacs require and load functions."
  :url "https://github.com/dholm/benchmark-init-el"
  :hook ((after-init . benchmark-init/deactivate))
  :init (benchmark-init/activate))

(straight-use-package 'gcmh)
(leaf gcmh
  :doc "Use GCMH --  the Garbage Collector Magic Hack -- to adjust garbage collection."
  :url "https://gitlab.com/koral/gcmh"
  :custom
  (gcmh-verbose             . t)
  (gcmh-lows-cons-threshold . #x800000)
  (gcmh-high-cons-threshold . gc-cons-threshold)
  (gcmh-idle-delay          . 3600)
  :config
  (gcmh-mode))

(straight-use-package 'no-littering)
(require 'no-littering)

(server-start)

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
