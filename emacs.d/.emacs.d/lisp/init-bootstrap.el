;;; init-bootstrap.el ---  bootstrap  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default debug-on-error         nil
              load-prefer-newer      t)

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

(leaf no-littering
  :straight t
  :require t)

;; https://github.com/abo-abo/hydra/wiki/
(leaf hydra
  :straight t
  :require t
  :bind ("H-x" . hydra-x/body)
  :hydra
  (hydra-x
   (:hint nil :exit t)
   "
--------------------------------------------------------------------
Manage repos: _u_pdate _c_ommit
Http servers: _d_ownloads _t_emp _s_creenshot _w_ork
     Browser: _lh_ 192.168.3.ip:port _lo_ 10.0.2.ip:port
        Apps: _j_ Day One _e_vernote
         Adb: _h_ome _o_ffice

Quit: _q_"
   ("u" (lambda ()
          (interactive)
          (async-shell-command "mr -d ~ update")
          (bookmark-maybe-load-default-file)))
   ("c" (lambda () (interactive) (async-shell-command "mr -d ~ commit")))
   ("d" (xr/change-hs-root "~/Downloads"))
   ("t" (xr/change-hs-root "~/temp"))
   ("s" (xr/change-hs-root "~/temp/screenshot"))
   ("w" (xr/change-hs-root "~/temp/donut"))
   ("h" (lambda () (interactive) (async-shell-command "~/Library/Android/sdk/platform-tools/adb connect 198.168.3.5")))
   ("o" (lambda (ip) (interactive "sIp: 10.0.2.") (async-shell-command (concat "~/Library/Android/sdk/platform-tools/adb connect 10.0.2." ip))))
   ("lh" (lambda () (interactive) (let ((ip (read-from-minibuffer "" "http://192.168.3.4")))
                                      (shell-command (concat "open " ip)))))
   ("lo" (lambda (ip) (interactive "s10.0.2.") (shell-command (concat "open http://10.0.2." ip))))
   ("j" (lambda () (interactive) (shell-command "open -a /Applications/Day\\ One.app")))
   ("e" (lambda () (interactive) (shell-command "open -a /Applications/Evernote.app")))
   ("q" nil)))

(provide 'init-bootstrap)
;;; init-bootstrap.el ends here
