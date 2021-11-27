;;; xr-skeleton.el --- Settings for skeletons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'skeleton
  (setq skeleton-end-newline nil))


;;; org

(define-skeleton xr--org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-skeleton xr--org-block-begin
  "Insert an org block begin, querying for type."
  "Type: "
  "#+begin_" str "\n")

(define-skeleton xr--org-block-end
  "Insert an org block end, querying for type."
  "Type: "
  "#+end_" str "\n")

(define-skeleton xr--org-block-verse
  "Insert an org verse block"
  ""
  "#+begin_verse\n"
  _ - \n
  "#+end_verse\n")

(define-skeleton xr--org-block-quote
  "Insert an org quote block"
  ""
  "#+begin_quote\n"
  _ - \n
  "#+end_quote\n")

(define-skeleton xr--org-block-comment
  "Insert an org comment block"
  ""
  "#+begin_comment\n"
  _ - \n
  "#+end_comment\n")

(define-skeleton xr--org-block-src
  "Insert an org src block, querying for type."
  "Language: "
  "#+begin_src " str "\n"
  _ - \n
  "#+end_src\n")

(define-skeleton xr--org-block-elisp
  "Insert an org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-skeleton xr--org-block-sh
  "Insert a org sh block"
  ""
  "#+begin_src shell\n"
  _ - \n
  "#+end_src\n")

(define-skeleton xr--org-block-kt
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src kotlin\n"
  _ - \n
  "#+end_src\n")

(define-skeleton xr--text-day
  "Insert a day info, like: 14(Fri)"
  ""
  (format-time-string "%d(%a)" (current-time)))

(define-skeleton xr--text-time
  "Insert a timestamp"
  ""
  (format-time-string "<%Y-%m-%d %a %R>" (current-time)))

(define-skeleton xr--weekly-review
  "Insert a heading for weekly review."
  ""
  (format-time-string "*** <%Y-%m-%d %R> W%y %b %W" (current-time)))

(define-skeleton xr--monthly-review
  "Insert a heading for monthly review."
  ""
  (format-time-string "*** <%Y-%m-%d %R> M%y %B %m" (current-time)))

(defhydra xr-hydra-skeleton-org
  (:hint nil :exit t :columns 4)
  "
Org Skeleton
"
  ("b" xr--org-block "block")
  (";" xr--org-block-comment "comment")
  ("q" xr--org-block-quote "quote")
  ("v" xr--org-block-verse "verse")
  ("w" xr-wrap-block "wrap block")
  ("c" xr--org-block-src "code")
  ("e" xr--org-block-elisp "elisp")
  ("s" xr--org-block-sh "sh")
  ("k" xr--org-block-kt "kotlin")
  ("r" xr--weekly-review "weekly review")
  ("m" xr--monthly-review "monthly review")
  ("t" xr--text-time "timestamp")
  ("d" xr--text-day "day: 14(Fri)"))
(define-key org-mode-map (kbd "M-t") #'xr-hydra-skeleton-org/body)

;;; elisp

(define-skeleton xr--el-template
  "Insert the elisp file template."
  ""
  ";;; " (buffer-name) " --- " (string-remove-suffix ".el" (buffer-name)) " -*- lexical-binding: t -*-\n"
  ";;; Commentary:\n"
  ";;; Code:\n\n"
  _ - \n
  "\n(provide '" (string-remove-suffix ".el" (buffer-name)) ")\n"
  ";;; " (buffer-name) " ends here")

(define-skeleton xr--el-lambdai
  "Insert an interactive lambda."
  ""
  "(lambda () (interactive) (" - "))")

(define-skeleton xr--el-autoload
  "Insert autoload comment."
  ""
  ";;;###autoload")

(defhydra xr-hydra-skeleton-elisp
  (:hint nil :exit t :columns 4)
  "
Elisp Skeleton
"
  ("a" xr--el-autoload "autoload comment")
  ("h" xr--el-template "header and footer")
  ("i" xr--el-lambdai "interactive lambda"))
(define-key emacs-lisp-mode-map (kbd "M-t") #'xr-hydra-skeleton-elisp/body)

;;; global

(define-skeleton xr--cmg-build-version
  "Build: Update app version to"
  ""
  "Build: Update app version to")

(define-skeleton xr--cmg-docs-changelog
  "Docs: Update changelog"
  ""
  "Docs: Update changelog")

(defhydra xr-hydra-skeleton-global
  (:hint nil :exit t :columns 4)
  "
Global Skeleton
"
  ("d" xr--cmg-docs-changelog "git changelog")
  ("v" xr--cmg-build-version "git app version")
  ("t" xr--text-time "timestamp"))
(global-set-key (kbd "M-t") #'xr-hydra-skeleton-global/body)

(provide 'xr-skeleton)
;;; xr-skeleton.el ends here
