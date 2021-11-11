;;; init-hydra.el --- init-hydra -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'hydra)

(defhydra hyper-x
  (:hint nil :exit t)
  "
--------------------------------------------------------------------
Manage repos: _u_pdate _c_ommit _r_estart after pull
Http servers: _d_ownloads _t_emp _s_creenshot _w_ork
     Browser: _lh_ 192.168.3.ip:port _lo_ 10.0.2.ip:port _x_ github.io
        Apps: _j_ Day One _e_vernote
         Adb: _h_ome _o_ffice

Quit: _q_"
  ("u"
   (lambda nil
     (interactive)
     (async-shell-command "mr -d ~ update")
     (bookmark-maybe-load-default-file)))
  ("r"
   (lambda nil
     (interactive)
     (shell-command "mr -d ~ update")
     (bookmark-maybe-load-default-file)
     (xr/restart-emacs)))
  ("c" (lambda nil (interactive) (async-shell-command "mr -d ~ commit")))
  ("d" (xr/change-hs-root "~/Downloads"))
  ("t" (xr/change-hs-root "~/temp"))
  ("s" (xr/change-hs-root "~/temp/screenshot"))
  ("w" (xr/change-hs-root "~/temp/donut"))
  ("h" (lambda nil (interactive) (async-shell-command "~/Library/Android/sdk/platform-tools/adb connect 198.168.3.5")))
  ("o"
   (lambda (ip)
     (interactive "sIp: 10.0.2.")
     (async-shell-command
      (concat "~/Library/Android/sdk/platform-tools/adb connect 10.0.2." ip))))
  ("lh"
   (lambda nil
     (interactive)
     (let ((ip (read-from-minibuffer "" "http://192.168.3.4")))
       (shell-command (concat "open " ip)))))
  ("lo" (lambda (ip) (interactive "s10.0.2.") (shell-command (concat "open http://10.0.2." ip))))
  ("j" (lambda nil (interactive) (shell-command "open -a /Applications/Day\\ One.app")))
  ("e" (lambda nil (interactive) (shell-command "open -a /Applications/Evernote.app")))
  ("x" (lambda nil (interactive) (shell-command "open  https://xandeer.github.io/20210629191000-000_index.html")))
  ("q" nil))

(global-set-key (kbd "H-x") 'hyper-x/body)

(defhydra hyper-j
   (:hint nil :exit t)
   "
   Agenda: _a_genda default _s_elect
       Xr: _d_elete current buffer _l_ remove links _c_convert quotations
           _f_ill subtree _j_ insert journal in year _m_igirage journal
 Bookmark: _e_motion _g_et up
   Others: _b_ookmark
   Cancel: _q_
"
   ("a" org-agenda-list)
   ("s" org-agenda)
   ("b" counsel-bookmark)
   ("e" (xr/bookmark "emotion"))
   ("g" (xr/bookmark "get_up"))
   ("c" xr/convert-chinese-quotations)
   ("d" xr/delete-current-buffer)
   ("f" xr/fill-subtree)
   ("l" xr/remove-links)
   ("j" xr/insert-journal-in-year)
   ("m" xr/migrate-journal)
   ("q" nil))
(global-set-key (kbd "H-j") 'hyper-j/body)

(provide 'init-hydra)
;;; init-hydra.el ends here
