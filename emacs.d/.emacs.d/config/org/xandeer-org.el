;;; xandeer-org.el --- Xandeer's emacs.d init org file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's emacs.d init org file.

;;; Code:


(straight-use-package 'deft)
(leaf deft
  :after org
  :bind
  ("C-c x d" . deft)
  :config
  (setq deft-directory org-directory)
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-recursive t)
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t))

(straight-register-package
   '(org-roam-server :host github
            :repo "org-roam/org-roam-server"
            :branch "master"
            :files (:defaults "README.md" "assets" "*.el" "index.html")))

  (straight-use-package 'org-roam-server)
  (leaf org-roam-server
    :require t
    :config
    (setq org-roam-server-host "0.0.0.0")
    (setq org-roam-server-port 8787)
    (setq org-roam-server-authenticate nil)
    (setq org-roam-server-export-inline-images t))

(provide 'xandeer-org)
;;; xandeer-org.el ends here
