;;; x-transients.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'transient)
(require 'consult)

(defmacro x/interactive-wrapper (form)
  "Create an interactive wrapper for a non-interactive FORM ."
  `(lambda ()
     (interactive)
     ,form))

(transient-define-prefix x/transient-x-group ()
  "Transient for x group commands."
  [["mr"
    ("u" "mr update" ,(x/interactive-wrapper (x/start-process "mr -d ~ update")))
    ("r" "restart after mr update" (lambda ()
                                     (interactive)
                                     (shell-command "mr -d ~ update")
                                     (bookmark-maybe-load-default-file)
                                     (x/restart-emacs)))
    ("c" "mr commit" (lambda ()
                       (interactive)
                       (recentf-save-list)
                       (x/start-process "mr -d ~ commit" t)))]
   ["Hs directories"
    ("hd" "hs downloads" ,(x/interactive-wrapper (x/change-hs-root "~/Downloads")))
    ("ht" "hs temp" ,(x/interactive-wrapper (x/change-hs-root "~/temp")))
    ("hs" "hs syncthing" ,(x/interactive-wrapper (x/change-hs-root "~/syncthing")))
    ("hp" "hs personal" ,(x/interactive-wrapper (x/change-hs-root "~/syncthing/personal")))
    ("hw" "hs work" ,(x/interactive-wrapper (x/change-hs-root "~/syncthing/donut")))]
   ["Others"
    ("a" "adb connect" x/sh-adb-connect)
    ("l" "open localhost" x/open-localhost)
    ("e" "eshell" eshell)
    ;; ("x" "open github.io" (x/open "https://xandeer.github.io/20210629191000-000_index.html"))
    ("x" "open second brain" ,(x/interactive-wrapper (x/open "https://mdk.vercel.app")))
    ("H-x" "launch another emacs" x/launch-separate-emacs-under-x)
    ("s" "eva query sleep" eva-query-sleep)
    ("H-r" "restart" x/restart-emacs)]])

(transient-define-prefix x/transient-buffer-group ()
  "Transient for opening buffers."
  [["Projectile"
    ("p" "Switch project" projectile-switch-project)
    ;; ("k" "Buffer" consult-buffer)
    ;; ("H-k" "Buffer other window" consult-buffer-other-window)
    ("w" "Projectile work" ,(x/interactive-wrapper (projectile-find-file-in-directory x/work-directory)))
    ("H-d" "Projectile dots" ,(x/interactive-wrapper (projectile-find-file-in-directory (x/expand-repo "dotfiles"))))
    ("H-f" "Project find file" projectile-find-file)
    ;; ("f" "Find file" find-file)
    ("e" "Open with external app" x/projectile-find-file-external)]
   ["Roam"
    ("H-j" "Yesterday" org-roam-dailies-goto-yesterday)
    ("r" "Roam random" org-roam-node-random :transient t)
    ("j" "Today" org-roam-dailies-goto-today)
    ("d" "Date" org-roam-dailies-goto-date)]
   ["Telega"
    ("H-s" "Telega root" x/open-telega-root)
    ("s" "Telega chat with" x/telega-chat-with)
    (">" "Send file to Telega" x/telega-send-to-chat)]
   ["Other"
    ("l" "Recent files" consult-recent-file)
    ("H-e" "Find library" find-library)]])

(defalias 'x/consult-dir #'consult-bookmark)
(consult-customize x/consult-dir :initial "dir: ")
(transient-define-prefix x/transient-position-group ()
  "Transient for dealing with special positions."
  [["Agenda"
    ("a" "Agenda all" ,(x/interactive-wrapper (org-agenda nil "x")))
    ("b" "Agenda book" ,(x/interactive-wrapper (org-agenda nil "b")))
    ("d" "Agenda daily" org-agenda-list)
    ("e" "Agenda emacs" ,(x/interactive-wrapper (org-agenda nil "e")))
    ("i" "Agenda inbox" ,(x/interactive-wrapper (org-agenda nil "i")))
    ("p" "Agenda personal" ,(x/interactive-wrapper (org-agenda nil "p")))
    ("t" "Agenda todo" ,(x/interactive-wrapper (org-agenda nil "t")))
    ("v" "Agenda list" org-agenda)
    ("w" "Agenda work" ,(x/interactive-wrapper (org-agenda nil "w")))
    ("H-i" "Consult agenda" consult-org-agenda)]
   ["Mark and register"
    ("f" "Mark" consult-mark)
    ("H-f" "Global mark" consult-global-mark)
    ("j" "Register" consult-register)
    ("H-j" "Register store" consult-register-store)
    ("l" "Open link" link-hint-open-link)
    ("k" "Consult bookmark" x/consult-bookmark)
    ("H-k" "Set bookmark" ,(x/interactive-wrapper
                            (if (equal major-mode 'org-mode)
                                (bookmark-set (org-roam--get-keyword "title"))
                              (bookmark-set))))

    ("H-h" "Bookmark dir" x/consult-dir)]])

(transient-define-prefix x/transient-global-group ()
  "Transient for global actions."
  [["Clock"
    ("i" "In last" org-clock-in-last)
    ("H-i" "In work chore" x/clock-in-work-chore-task)
    ("j" "Goto current" org-clock-goto)
    ("H-l" "In organization" bh/clock-in-organization-task-as-default)
    ("l" "Out" org-clock-out)
    ("k" "Done current" x/org-done-current)]
   ["Mics"
    ("n" "Focus lines" consult-focus-lines)
    ("o" "Consult outline" consult-outline)

    ("c" "Convert quotations" x/convert-to-chinese-quotations)
    ("d" "Delete current buffer" x/delete-current-buffer)
    ("H-d" "Duplicate line" x/duplicate-line)]])

(transient-define-prefix x/transient-search-group ()
  "Transient for searching."
  [["Search engine"
    ("b" "Zlib" engine/search-zlib)
    ("c" "Grep.app" engine/search-grep-app)
    ("g" "Google" engine/search-google)
    ("u" "GitHub" engine/search-github)
    ("w" "Wiki" engine/search-wiki-en)
    ("z" "Wiki CN" engine/search-wiki-cn)]
   ["Dictionary or translator"
    ("l" "Lookup point" sdcv-search-pointer)
    ("H-l" "Lookup input" sdcv-search-input)
    ("k" "Translate" gts-do-translate)
    ("i" "TTS EN" azure-tts-play-region-english)
    ("H-i" "TTS ZH" azure-tts-play-region-chinese)]
   ["Consult rg"
    ("s" "Rg current directory" ,(x/interactive-wrapper (consult-ripgrep default-directory)))
    ("H-s" "Rg project" consult-ripgrep)]])

(x/define-keys
 global-map
 '(("H-x" x/transient-x-group)
   ("H-d" x/transient-global-group)
   ("H-f" x/transient-buffer-group)
   ("H-j" x/transient-position-group)
   ("H-s" x/transient-search-group)))

(provide 'x-transients)
;;; x-transients.el ends here
