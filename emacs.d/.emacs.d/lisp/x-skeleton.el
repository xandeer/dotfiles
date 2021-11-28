;;; x-skeleton.el --- Settings for skeletons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'skeleton
  (setq skeleton-end-newline nil))


;;; org

(define-skeleton x--org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-skeleton x--org-block-begin
  "Insert an org block begin, querying for type."
  "Type: "
  "#+begin_" str "\n")

(define-skeleton x--org-block-end
  "Insert an org block end, querying for type."
  "Type: "
  "#+end_" str "\n")

(define-skeleton x--org-block-verse
  "Insert an org verse block"
  ""
  "#+begin_verse\n"
  _ - \n
  "#+end_verse\n")

(define-skeleton x--org-block-quote
  "Insert an org quote block"
  ""
  "#+begin_quote\n"
  _ - \n
  "#+end_quote\n")

(define-skeleton x--org-block-comment
  "Insert an org comment block"
  ""
  "#+begin_comment\n"
  _ - \n
  "#+end_comment\n")

(define-skeleton x--org-block-src
  "Insert an org src block, querying for type."
  "Language: "
  "#+begin_src " str "\n"
  _ - \n
  "#+end_src\n")

(define-skeleton x--org-block-elisp
  "Insert an org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-skeleton x--org-block-sh
  "Insert a org sh block"
  ""
  "#+begin_src shell\n"
  _ - \n
  "#+end_src\n")

(define-skeleton x--org-block-kt
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src kotlin\n"
  _ - \n
  "#+end_src\n")

(define-skeleton x--text-day
  "Insert a day info, like: 14(Fri)"
  ""
  (format-time-string "%d(%a)" (current-time)))

(define-skeleton x--text-time
  "Insert a timestamp"
  ""
  (format-time-string "<%Y-%m-%d %a %R>" (current-time)))

(define-skeleton x--weekly-review
  "Insert a heading for weekly review."
  ""
  (format-time-string "*** <%Y-%m-%d %R> W%y %b %W" (current-time)))

(define-skeleton x--monthly-review
  "Insert a heading for monthly review."
  ""
  (format-time-string "*** <%Y-%m-%d %R> M%y %B %m" (current-time)))

(defhydra x/hydra-skeleton-org
  (:hint nil :exit t :columns 4)
  "
Org Skeleton
"
  ("b" x--org-block "block")
  (";" x--org-block-comment "comment")
  ("q" x--org-block-quote "quote")
  ("v" x--org-block-verse "verse")
  ("w" x/wrap-block "wrap block")
  ("c" x--org-block-src "code")
  ("e" x--org-block-elisp "elisp")
  ("s" x--org-block-sh "sh")
  ("k" x--org-block-kt "kotlin")
  ("r" x--weekly-review "weekly review")
  ("m" x--monthly-review "monthly review")
  ("t" x--text-time "timestamp")
  ("d" x--text-day "day: 14(Fri)"))
(define-key org-mode-map (kbd "M-t") #'x/hydra-skeleton-org/body)

;;; elisp

(define-skeleton x--el-template
  "Insert the elisp file template."
  ""
  ";;; " (buffer-name) " --- " (string-remove-suffix ".el" (buffer-name)) " -*- lexical-binding: t -*-\n"
  ";;; Commentary:\n"
  ";;; Code:\n\n"
  _ - \n
  "\n(provide '" (string-remove-suffix ".el" (buffer-name)) ")\n"
  ";;; " (buffer-name) " ends here")

(define-skeleton x--el-lambdai
  "Insert an interactive lambda."
  ""
  "(lambda () (interactive) (" - "))")

(define-skeleton x--el-autoload
  "Insert autoload comment."
  ""
  ";;;###autoload")

(define-skeleton x--el-global-set-key
  "Global set key."
  ""
  "(global-set-key (kbd \"" - "\") #')")

(defhydra x/hydra-skeleton-elisp
  (:hint nil :exit t :columns 4)
  "
Elisp Skeleton
"
  ("a" x--el-autoload "autoload comment")
  ("g" x--el-global-set-key "global set key")
  ("h" x--el-template "header and footer")
  ("i" x--el-lambdai "interactive lambda"))
(define-key emacs-lisp-mode-map (kbd "M-t") #'x/hydra-skeleton-elisp/body)

;;; global

(define-skeleton x--cmg-build-version
  "Build: Update app version to"
  ""
  "Build: Update app version to")

(define-skeleton x--cmg-docs-changelog
  "Docs: Update changelog"
  ""
  "Docs: Update changelog")

(defhydra x/hydra-skeleton-global
  (:hint nil :exit t :columns 4)
  "
Global Skeleton
"
  ("d" x--cmg-docs-changelog "git changelog")
  ("v" x--cmg-build-version "git app version")
  ("t" x--text-time "timestamp"))
(global-set-key (kbd "M-t") #'x/hydra-skeleton-global/body)

(provide 'x-skeleton)
;;; x-skeleton.el ends here
