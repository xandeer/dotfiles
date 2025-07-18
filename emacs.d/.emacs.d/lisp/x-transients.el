;;; x-transients.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'transient)
(require 'consult)

;; workaround for transient.el bug static-if
(defun transient--wrap-command ()
  (letrec
      ((prefix transient--prefix)
       (suffix this-command)
       (advice
        (lambda (fn &rest args)
          (interactive
           (lambda (spec)
             (let ((abort t))
               (unwind-protect
                   (prog1 (let ((debugger #'transient--exit-and-debug))
                            (advice-eval-interactive-spec spec))
                     (setq abort nil))
                 (when abort
                   (when-let ((unwind (oref prefix unwind-suffix)))
                     (transient--debug 'unwind-interactive)
                     (funcall unwind suffix))
                   (advice-remove suffix advice)
                   (oset prefix unwind-suffix nil))))))
          (unwind-protect
              (let ((debugger #'transient--exit-and-debug))
                (apply fn args))
            (when-let ((unwind (oref prefix unwind-suffix)))
              (transient--debug 'unwind-command)
              (funcall unwind suffix))
            (advice-remove suffix advice)
            (oset prefix unwind-suffix nil)))))
    (when (symbolp this-command)
      (advice-add suffix :around advice '((depth . -99))))
    (cl-assert
     (>= emacs-major-version 30) nil
     "Emacs was downgraded, making it necessary to recompile Transient")))

(defmacro x/interactive-wrapper (form)
  "Create an interactive wrapper for a non-interactive FORM ."
  `(lambda ()
     (interactive)
     ,form))

(transient-define-prefix x/transient-x-group ()
  "Transient for x group commands."
  [["mr"
    ;; ("u" "mr update" ,(x/interactive-wrapper) (x/start-process "mr -d ~ update"))
    ("u" "mr update" (lambda () (interactive) (x/start-process "mr -d ~ update")))
    ("H-u" "reload bookmarks & revert org buffers" (lambda ()
                         (interactive)
                         (x/update-bookmarks)
                         (org-revert-all-org-buffers)))
    ("r" "restart after mr update" (lambda ()
                                     (interactive)
                                     (shell-command "mr -d ~ update")
                                     (bookmark-maybe-load-default-file)
                                     (restart-emacs)))
    ("c" "mr commit" (lambda ()
                       (interactive)
                       (recentf-save-list)
                       (x/start-process "mr -d ~ commit" t)))]
   ["Hs directories"
    ("hd" "hs downloads" (lambda () (interactive) (x/change-hs-root "~/Downloads")))
    ("ht" "hs temp" (lambda () (interactive) (x/change-hs-root "~/temp")))
    ("hs" "hs syncthing" (lambda () (interactive) (x/change-hs-root "~/syncthing")))
    ("hp" "hs personal" (lambda () (interactive) (x/change-hs-root "~/syncthing/personal")))]
   ["Others"
    ("a" "adb connect" x/sh-adb-connect)
    ("l" "open localhost" x/open-localhost)
    ("e" "eat eshell" eshell)
    ;; ("x" "open github.io" (x/open "https://xandeer.github.io/20210629191000-000_index.html"))
    ("x" "open second brain" (lambda () (interactive) (x/open "https://mdk.vercel.app")))
    ("H-x" "launch another emacs" x/launch-separate-emacs-under-x)
    ;; ("s" "eva query sleep" eva-query-sleep)
    ("H-r" "restart" restart-emacs)]])

(defun x/open-with-cursor ()
  (interactive)
  (x/start-process (format "cursor %s" buffer-file-name)))

(transient-define-prefix x/transient-buffer-group ()
  "Transient for opening buffers."
  [["Projectile"
    ("p" "Switch project" projectile-switch-project)
    ;; ("k" "Buffer" consult-buffer)
    ;; ("H-k" "Buffer other window" consult-buffer-other-window)
    ("b" "Projectile book" (lambda () (interactive)  (projectile-find-file-in-directory (expand-file-name "syncthing/personal/book" "~"))))
    ("n" "Projectile notes" (lambda () (interactive)  (projectile-find-file-in-directory org-directory)))
    ("H-d" "Projectile dots" (lambda () (interactive)  (projectile-find-file-in-directory (x/expand-repo "dotfiles"))))
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
    ("H-e" "Find library" find-library)
    ("c" "Open with Cursor" x/open-with-cursor)]])

(defalias 'x/consult-dir #'consult-bookmark)
(consult-customize x/consult-dir :initial "dir: ")
(transient-define-prefix x/transient-position-group ()
  "Transient for dealing with special positions."
  [["Agenda"
    ("a" "Agenda all" (lambda () (interactive) (org-agenda nil "x")))
    ("b" "Agenda book" (lambda () (interactive) (org-agenda nil "b")))
    ("d" "Agenda daily" org-agenda-list)
    ("e" "Agenda emacs" (lambda () (interactive) (org-agenda nil "e")))
    ("i" "Agenda inbox" (lambda () (interactive) (org-agenda nil "i")))
    ("m" "Agenda MoonDust" (lambda () (interactive) (org-agenda nil "m")))
    ("p" "Agenda personal" (lambda () (interactive) (org-agenda nil "p")))
    ("r" "Agenda MoonReader" (lambda () (interactive) (org-agenda nil "r")))
    ("t" "Agenda todo" (lambda () (interactive) (org-agenda nil "t")))
    ("w" "Agenda HoldLines" (lambda () (interactive) (org-agenda nil "w")))
    ("v" "Agenda list" org-agenda)
    ("H-i" "Consult agenda" consult-org-agenda)]
   ["Mark and register"
    ("f" "Mark" consult-mark)
    ("H-f" "Global mark" consult-global-mark)
    ("j" "Register" consult-register)
    ("H-j" "Register store" consult-register-store)
    ("l" "Open link" link-hint-open-link)
    ("H-l" "Open link" (lambda () (interactive)
                       (let ((browse-url-browser-function 'browse-url-default-browser))
                         (link-hint-open-link))))
    ("k" "Consult bookmark" x/consult-bookmark)
    ("H-k" "Set bookmark" (lambda () (interactive) (if (equal major-mode 'org-mode)
                                                       (bookmark-set (org-roam--get-keyword "title"))
                                                     (bookmark-set))))

    ("H-h" "Bookmark dir" x/consult-dir)]])

(transient-define-prefix x/transient-global-group ()
  "Transient for global actions."
  [["Clock"
    ("i" "In last" org-clock-in-last)
    ("g" "In organization" x/clock-in-organization-task)
    ("w" "In working" x/clock-in-working-task)
    ("r" "In reading" x/clock-in-reading-task)
    ("n" "In noting" x/clock-in-noting-task)
    ("l" "Out" org-clock-out)
    ("j" "Goto current" org-clock-goto)
    ("k" "Done current" x/org-done-current)]
   ["Mics"
    ("f" "Focus lines" consult-focus-lines)
    ("o" "Consult outline" consult-outline)

    ("c" "Convert quotations" x/convert-to-chinese-quotations)
    ("d" "Delete current buffer" x/delete-current-buffer)
    ("H-d" "Duplicate line" x/duplicate-line)]])

(autoload 'consult-org-roam-search "consult-org-roam" nil t)
(transient-define-prefix x/transient-search-group ()
  "Transient for searching."
  [["Search engine"
    ("p" "perplexity" engine/search-perplexity)
    ("b" "Zlib" engine/search-zlib)
    ("c" "Grep.app" engine/search-grep-app)
    ("g" "Google" engine/search-google)
    ("u" "GitHub" engine/search-github)
    ("w" "Wiki" engine/search-wiki-en)
    ("z" "Wiki CN" engine/search-wiki-cn)]
   ["Dictionary or translator"
    ("l" "Lookup point" sdcv-search-pointer)
    ("H-l" "Lookup input" sdcv-search-input)
    ("k" "Translate" gt-do-translate)
    ("i" "TTS" azure-tts-play-region)
    ("H-i" "TTS replay" azure-tts-replay)]
   ["Consult"
    ("a" "agenda" consult-org-agenda)
    ("d" "dir" consult-dir)
    ("r" "roam search" consult-org-roam-search)
    ("s" "rg current directory" (lambda () (interactive) (consult-ripgrep default-directory)))
    ("H-s" "rg project" consult-ripgrep)]])

(x/define-keys
 global-map
 '(("H-x" x/transient-x-group)
   ("H-d" x/transient-global-group)
   ("H-f" x/transient-buffer-group)
   ("H-j" x/transient-position-group)
   ("H-s" x/transient-search-group)))

(provide 'x-transients)
;;; x-transients.el ends here
