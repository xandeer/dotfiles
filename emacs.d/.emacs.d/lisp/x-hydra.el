;;; x-hydra.el --- x-hydra -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defhydra x/hydra-x (:exit t :columns 4 :idle 0.3)
  ""
  ("u" (x/start-process "mr -d ~ update") "mr update")
  ("r"
   (lambda nil
     (interactive)
     (shell-command "mr -d ~ update")
     (bookmark-maybe-load-default-file)
     (x/restart-emacs)) "restart after mr update")
  ("c" (lambda () (interactive) (recentf-save-list) (x/start-process "mr -d ~ commit")) "mr commit")
  ("H-r" x/restart-emacs "restart")
  ("hd" (x/change-hs-root "~/Downloads") "hs downloads")
  ("ht" (x/change-hs-root "~/temp") "hs temp")
  ("hs" (x/change-hs-root "~/syncthing") "hs syncthing")
  ("hp" (x/change-hs-root "~/syncthing/personal") "hs personal")
  ("hw" (x/change-hs-root "~/syncthing/donut") "hs work")
  ("a" x/sh-adb-connect "adb connect")
  ("l" (lambda (ip)
         (interactive (list (read-string "Ip: " (x/ifconfig))))
         (x/open (concat "http://" ip))) "open localhost")
  ;; ("j" (x/open-with "/Applications/Day\\ One.app"))
  ;; ("e" (x/open "/Applications/Evernote.app") "open evernote")
  ("e" eshell "eshell")
  ("x" (x/open "https://xandeer.github.io/20210629191000-000_index.html") "open github.io")
  ("s" #'eva-query-sleep "eva query sleep"))

(defhydra x/hydra-open-buffer (:exit t :columns 4 :idle 0.3)
  "
Buffer\n"
  ("p" projectile-switch-project "switch project")
  ("k" consult-buffer "buffer")
  ("H-k" consult-buffer-other-window "buffer other window")
  ("r" org-roam-node-random "roam random" :exit nil)
  ("j" org-roam-dailies-goto-today "today")
  ("H-j" org-roam-dailies-goto-yesterday "yesterday")
  ("d" org-roam-dailies-goto-date "date")
  ("H-d" (projectile-find-file-in-directory (x/expand-repo "dotfiles")) "projectile dots")
  ("w" (projectile-find-file-in-directory x/work-directory) "projectile work")
  ("f" find-file "find file")
  ("H-f" projectile-find-file "project find file")
  ("l" consult-recent-file "recent files")
  ("s" x/telega-chat-with "telega chat with")
  ("H-s" x/open-telega-root "telega root")
  (">" x/telega-send-to-chat "send file to telega"))

(global-set-key (kbd "H-x") 'x/hydra-x/body)
(global-set-key (kbd "H-f") #'x/hydra-open-buffer/body)

(defun x--bookmark-set ()
  (interactive)
  (if (equal major-mode 'org-mode)
      (bookmark-set (org-roam--get-keyword "title"))
    (bookmark-set)))

(defhydra x/hydra-deal-special-position (:exit t :columns 4 :idle 0.3)
  ""
  ("f" consult-mark "mark")
  ("H-f" consult-global-mark "global mark")
  ("j" consult-register "register")
  ("H-j" consult-register-store "register store")
  ("l" link-hint-open-link "open link")
  ("k" x/consult-bookmark "consult bookmark")
  ("H-k" x--bookmark-set "set bookmark")

  ("d" org-agenda-list "agenda daily")
  ("a" (org-agenda nil "x") "agenda all")
  ("e" (org-agenda nil "e") "agenda emacs")
  ("i" (org-agenda nil "i") "agenda inbox")
  ("t" (org-agenda nil "t") "agenda todo")
  ("p" (org-agenda nil "p") "agenda personal")
  ("v" org-agenda "agenda list")
  ("w" (org-agenda nil "w") "agenda work")

  ("H-h" (dired "~") "home")
  ("H-d" (dired "~/Downloads") "downloads")
  ("H-e" (dired "~/.emacs.d") ".emacs.d")
  ("H-t" (dired "~/syncthing/personal/telega/documents") "telega")
  ("H-s" (dired "~/syncthing/personal") "syncthing personal")
  ("H-w" (dired "~/syncthing/donut") "work temp")
  ("H-n" (dired org-directory) "notes"))

(global-set-key (kbd "H-j") #'x/hydra-deal-special-position/body)

(defhydra x/hydra-global-actions (:exit t :columns 4 :idle 0.3)
  ""
  ("n" consult-focus-lines "focus lines")
  ("o" consult-outline "consult outline")

  ("c" x/convert-chinese-quotations "convert quotations")
  ("d" x/delete-current-buffer "delete current buffer")
  ("H-d" x/duplicate-line "duplicate line")

  ("k" x/org-done-current "done current")
  ("j" org-clock-goto "goto current")
  ("i" org-clock-in-last "in last")
  ("H-i" x/clock-in-work-chore-task "in work chore")
  ("H-l" bh/clock-in-organization-task-as-default "in organization")
  ("l" org-clock-out "out"))

(global-set-key (kbd "H-d") #'x/hydra-global-actions/body)
(global-set-key (kbd "M-k") #'x/switch-to-last-buffer)
(global-set-key (kbd "H-c") #'org-capture)

(defhydra x/hydra-search (:exit t :columns 4 :idle 0.3)
  ""
  ("s" (consult-ripgrep default-directory) "rg current directory")
  ("H-s" consult-ripgrep "rg project")
  ("e" find-library "find library")
  ("p" projectile-switch-project "switch project")
  ("u" engine/search-github "github")
  ("g" engine/search-google "google")
  ("c" engine/search-grep-app "grep.app")
  ("w" engine/search-wiki-en "wiki")
  ("z" engine/search-wiki-cn "wiki cn")
  ("l" sdcv-search-pointer "lookup point")
  ("H-l" sdcv-search-input "lookup input")
  ("k" gts-do-translate "translate"))

(global-set-key (kbd "H-s") #'x/hydra-search/body)

(provide 'x-hydra)
;;; x-hydra.el ends here
