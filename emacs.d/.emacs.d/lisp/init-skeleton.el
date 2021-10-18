;;; init-skeleton.el --- Settings for skeletons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf skeleton
  :after hydra
  :config
  (setq skeleton-end-newline nil)

  ;; Skeletons
  (define-skeleton xr/org-block
    "Insert an org block, querying for type."
    "Type: "
    "#+begin_" str "\n"
    _ - \n
    "#+end_" str "\n")

  (define-skeleton xr/org-block-begin
    "Insert an org block begin, querying for type."
    "Type: "
    "#+begin_" str "\n")

  (define-skeleton xr/org-block-end
    "Insert an org block end, querying for type."
    "Type: "
    "#+end_" str "\n")

  (define-skeleton xr/org-block-verse
    "Insert an org verse block"
    ""
    "#+begin_verse\n"
    _ - \n
    "#+end_verse\n")

  (define-skeleton xr/org-block-quote
    "Insert an org quote block"
    ""
    "#+begin_quote\n"
    _ - \n
    "#+end_quote\n")

  (define-skeleton xr/org-block-comment
    "Insert an org comment block"
    ""
    "#+begin_comment\n"
    _ - \n
    "#+end_comment\n")

  (define-skeleton xr/org-block-src
    "Insert an org src block, querying for type."
    "Language: "
    "#+begin_src " str "\n"
    _ - \n
    "#+end_src\n")

  (define-skeleton xr/org-block-elisp
    "Insert an org emacs-lisp block"
    ""
    "#+begin_src emacs-lisp\n"
    _ - \n
    "#+end_src\n")

  (define-skeleton xr/org-block-sh
    "Insert a org sh block"
    ""
    "#+begin_src shell\n"
    _ - \n
    "#+end_src\n")

  (define-skeleton xr/org-block-kt
    "Insert a org emacs-lisp block"
    ""
    "#+begin_src kotlin\n"
    _ - \n
    "#+end_src\n")

  (define-skeleton xr/text-day
    "Insert a day info, like: 14(Fri)"
    ""
    (format-time-string "%d(%a)" (current-time)))

  (define-skeleton xr/text-time
    "Insert a timestamp"
    ""
    (format-time-string "<%Y-%m-%d %a %R>" (current-time)))

  (define-skeleton xr/el-template
    "Insert the elisp file template."
    ""
    ";;; " (buffer-name) " --- " (string-remove-suffix ".el" (buffer-name)) " -*- lexical-binding: t -*-\n"
    ";;; Commentary:\n"
    ";;; Code:\n\n"
    _ - \n
    "\n(provide '" (string-remove-suffix ".el" (buffer-name)) ")\n"
    ";;; " (buffer-name) " ends here")

  (define-skeleton xr/el-lambdai
    "Insert an interactive lambda."
    ""
    "(lambda () (interactive) (" - "))")

  (define-skeleton xr/weekly-review
    "Insert a heading for weekly review."
    ""
    (format-time-string "*** <%Y-%m-%d %R> W%y %W %b" (current-time)))

  (define-skeleton xr/monthly-review
    "Insert a heading for monthly review."
    ""
    (format-time-string "*** <%Y-%m-%d %R> M%y %B %m" (current-time)))
  :bind*
  ("C-c M-x" . xr/hydra-skeleton/body)
  :hydra
  (xr/hydra-skeleton
   nil
   "Insert Skeleton"
   ("b" xr/org-block "block" :exit t)
   ("c" xr/org-block-src "code" :exit t)
   ("d" xr/text-day "day: 14(Fri)" :exit t)
   ("e" xr/org-block-elisp "elisp" :exit t)
   ("h" xr/el-template "elisp template" :exit t)
   ("s" xr/org-block-sh "sh" :exit t)
   ("k" xr/org-block-kt "kotlin" :exit t)
   ("t" xr/text-time "timestamp" :exit t)))

(leaf abbrev
  :after skeleton
  :hook
  ((fundamental-mode-hook org-mode-hook emacs-lisp-mode-hook text-mode-hook) . abbrev-mode)
  :config
  (define-abbrev fundamental-mode-abbrev-table "tt" "" 'xr/text-time)
  (define-abbrev org-mode-abbrev-table "tt" "" 'xr/text-time)
  (define-abbrev org-mode-abbrev-table "bb" "" 'xr/org-block)
  (define-abbrev org-mode-abbrev-table "bq" "#+begin_quote")
  (define-abbrev org-mode-abbrev-table "eq" "#+end_quote")
  (define-abbrev org-mode-abbrev-table "bv" "#+begin_verse")
  (define-abbrev org-mode-abbrev-table "ev" "#+end_verse")
  (define-abbrev org-mode-abbrev-table "cc" "" 'xr/org-block-comment)
  (define-abbrev org-mode-abbrev-table "qq" "" 'xr/org-block-quote)
  (define-abbrev org-mode-abbrev-table "vv" "" 'xr/org-block-verse)
  (define-abbrev org-mode-abbrev-table "src" "" 'xr/org-block-src)
  (define-abbrev org-mode-abbrev-table "dd" "" 'xr/text-day)
  (define-abbrev org-mode-abbrev-table "el" "" 'xr/org-block-elisp)
  (define-abbrev org-mode-abbrev-table "sh" "" 'xr/org-block-sh)
  (define-abbrev org-mode-abbrev-table "kt" "" 'xr/org-block-kt)
  (define-abbrev org-mode-abbrev-table "ww" "" 'xr/weekly-review)
  (define-abbrev org-mode-abbrev-table "mm" "" 'xr/monthly-review)
  (define-abbrev text-mode-abbrev-table "gv" "Build: Update app version to")
  (define-abbrev text-mode-abbrev-table "gd" "Docs: Update changelog")
  (define-abbrev emacs-lisp-mode-abbrev-table "lambdai" "" 'xr/el-lambdai)
  (define-abbrev emacs-lisp-mode-abbrev-table "elh" "" 'xr/el-template))

(provide 'init-skeleton)
;;; init-skeleton.el ends here
