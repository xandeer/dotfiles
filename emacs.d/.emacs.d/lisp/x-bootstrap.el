;;; x-bootstrap.el ---  bootstrap  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default load-prefer-newer t)

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "localhost:8010")
     ("https" . "localhost:8010")))

(setq straight-recipes-gnu-elpa-use-mirror    t
      straight-repository-branch              "develop"
      straight-vc-git-default-clone-depth     1
      straight-enable-use-package-integration nil
      straight-check-for-modifications        '(find-when-checking))

(setq warning-suppress-types '((straight)))

(defvar bootstrap-version)

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      `((nil . ,(expand-file-name "etc/packages-default.el" vanilla-path))
        ;; Packages which are pinned to a specific commit.
        (pinned . ,(expand-file-name "etc/packages-pinned.el" vanilla-path))))

(unless (boundp 'doom-version)
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
    (load bootstrap-file nil 'nomessage)))

(defun require-package (package &optional require)
  "Just wrap PACKAGE with `straight-use-package`.
When REQUIRE is `t`, require the PACKAGE."
  (straight-use-package package)
  (when require
    (require package)))

(defun x/straight--fix-build (recipe &optional cause)
  (shell-command "gsed -i 's#../../../../../../../../.emacs.d#/Users/kevin/.emacs.d#g' ~/.emacs.d/straight/build/*/*autoloads.el"))
(unless (boundp 'doom-version)
  ;; (advice-add 'straight--build-package :after #'x/straight--fix-build)
  )

(provide 'x-bootstrap)
;;; x-bootstrap.el ends here
