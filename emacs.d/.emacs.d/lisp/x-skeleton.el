;;; x-skeleton.el --- Settings for skeletons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'skeleton
  (setq skeleton-end-newline nil))


;;; org

(define-skeleton x--org-block-sh
  "Insert a org sh block"
  ""
  "#+begin_src sh\n"
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
  (format-time-string "*** <%Y-%m-%d %R> W%y %b %U " (current-time))
  _ - \n
  "**** Review\n1. ")

(define-skeleton x--monthly-review
  "Insert a heading for monthly review."
  ""
  (format-time-string "*** <%Y-%m-%d %R> M%y %B %m" (current-time)))

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

(define-skeleton x--el-define-skeleton
  "Define skeleton."
  "Name: "
  "(define-skeleton x--" str
  "\n\t\".\"\n\t"
  "\"\"\n\t"
  _ - ")")

(define-skeleton x--el-defhydra
	"Defhydra."
	"Name: "
	"(defhydra x-hydra-" str
  " (:exit t :columns 4 :idle 0.3)"
  "\n\t\"\"\n\t"
  _ - ")")

(defhydra x/hydra-skeleton-elisp (:exit t :columns 4 :idle 0.3)
  "
Elisp Skeleton
"
  ("ds" x--el-define-skeleton "define skeleton")
  ("dh" x--el-defhydra "defhydra")
  ("a" x--el-autoload "autoload comment")
  ("g" x--el-global-set-key "global set key")
  ("h" (tempel-insert 'file-head) "header and footer")
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

(defhydra x/hydra-skeleton-global (:exit t :columns 4 :idle 0.3)
  "
Global Skeleton
"
  ("d" x--cmg-docs-changelog "git changelog")
  ("v" x--cmg-build-version "git app version")
  ("t" x--text-time "timestamp"))
(global-set-key (kbd "M-t") #'x/hydra-skeleton-global/body)

(provide 'x-skeleton)
;;; x-skeleton.el ends here
