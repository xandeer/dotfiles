;;; x-hydra.el --- x-hydra -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'hydra)

;;; x

(defhydra x-hydra-x
  (:exit t :columns 4)
  ""
  ("u"
   (lambda nil
     (interactive)
     (shell-command "mr -d ~ update")
     (bookmark-maybe-load-default-file)) "mr update")
  ("r"
   (lambda nil
     (interactive)
     (shell-command "mr -d ~ update")
     (bookmark-maybe-load-default-file)
     (x/restart-emacs)) "restart after mr update")
  ("c" (x/async-command "mr -d ~ commit") "mr commit")
  ("H-r" x/restart-emacs "restart")
  ("hd" (x/change-hs-root "~/Downloads") "hs downloads")
  ("ht" (x/change-hs-root "~/temp") "hs temp")
  ("hs" (x/change-hs-root "~/temp/screenshot") "hs screenshot")
  ("hw" (x/change-hs-root "~/temp/donut") "hs work")
  ("a"
   (lambda (ip)
     (interactive (list (read-string "Ip: " (concat (s-join "." (butlast (s-split "\\." (x/ifconfig)))) "."))))
     (x/async-command
      (concat "~/Library/Android/sdk/platform-tools/adb connect " ip))) "adb connect")
  ("l" (lambda (ip)
          (interactive (list (read-string "Ip: " (x/ifconfig))))
          (x/open (concat "http://" ip))) "open localhost")
  ;; ("j" (x/open-with "/Applications/Day\\ One.app"))
  ("e" (x/open-with "/Applications/Evernote.app") "open evernote")
  ("x" (x/open "https://xandeer.github.io/20210629191000-000_index.html") "open github.io"))

(global-set-key (kbd "H-x") 'x-hydra-x/body)

;;; jump

(defhydra x-hydra-jump
  (:exit t :columns 4)
  ""
  ("b" consult-buffer "buffer")
  ("H-b" consult-buffer-other-window "buffer other window")
  ("m" consult-bookmark "consult bookmark")
  ("H-r" consult-register-store "register store")
  ("r" consult-register "register")
  ("n" consult-focus-lines "focus lines")
  ("f" find-file "find file")
  ("H-f" projectile-find-file "project find file")
  ("o" consult-outline "consult outline")
  ("p" projectile-switch-project "switch project")
  ("l" link-hint-open-link "open link")
  ("t" x/telega-chat-with "telega chat with")
  ("e" (x/bookmark "emotion") "emotion")
  ("g" (x/bookmark "get_up") "get up")
  ("c" x/convert-chinese-quotations "convert quotations")
  ("d" x/delete-current-buffer "delete current buffer"))
(global-set-key (kbd "H-j") 'x-hydra-jump/body)

(global-set-key (kbd "M-k") #'x/switch-to-last-buffer)

;;; search

(defhydra x-hydra-search (:exit t :columns 4)
  ""
  ("s" (consult-ripgrep default-directory) "rg current directory")
  ("H-s" consult-ripgrep "rg project")
  ("p" projectile-switch-project "switch project")
  ("f" find-file "find file")
  ("H-f" projectile-find-file "project find file")
  ("b" consult-buffer "buffer")
  ("H-b" consult-buffer-other-window "buffer other window")
  ("r" consult-recent-file "recent files")
  ("u" engine/search-github "github")
  ("g" engine/search-google "google")
  ("c" engine/search-grep-app "grep.app")
  ("w" engine/search-wiki-en "wiki")
  ("z" engine/search-wiki-cn "wiki cn")
  ("l" sdcv-search-pointer "lookup point")
  ("k" sdcv-search-input "lookup input")
  ("t" go-translate "translate"))

(global-set-key (kbd "H-s") #'x-hydra-search/body)

(provide 'x-hydra)
;;; x-hydra.el ends here
