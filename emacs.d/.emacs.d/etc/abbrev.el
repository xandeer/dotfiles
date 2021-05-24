;;-*-coding: utf-8;-*-
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("h" "" xr/el-template :count 1)
   ))

(define-abbrev-table 'fundamental-mode-abbrev-table
  '(
    ("t" "" xr/text-time :count 0)
    ("xt" "" xr/text-time :count 0)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("b" "" xr/org-block :count 1)
    ("d" "" xr/text-day :count 2)
    ("el" "" xr/org-block-elisp :count 1)
    ("kt" "" xr/org-block-kt :count 0)
    ("sh" "" xr/org-block-sh :count 1)
    ("src" "" xr/org-block-src :count 1)
    ("t" "" xr/text-time :count 2)
    ("xc" "" xr/org-block-src :count 0)
    ("xd" "" xr/text-day :count 3)
    ("xel" "" xr/org-block-elisp :count 1)
    ("xkt" "" xr/org-block-kt :count 0)
    ("xsh" "" xr/org-block-sh :count 0)
    ("xx" "" xr/org-block :count 1)
   ))

(define-abbrev-table 'prog-mode-abbrev-table
  '(
    ("t" "" xr/text-time :count 0)
   ))

