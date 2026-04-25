#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
x_org_capture_el="$repo_root/emacs.d/.emacs.d/lisp/x-org-capture.el"

[[ -f "$x_org_capture_el" ]] || {
  print -u2 "expected repository-managed Emacs org-capture module at $x_org_capture_el"
  exit 1
}

output="$(
  emacs --batch -Q \
    --eval "(progn
              (require 'org-capture)
              (require 'org)
              (defun x/define-keys (&rest _args) nil)
              (defun x/insert-weather () nil)
              (defvar x/org-today-tag \"today\")
              (defvar org-journal-time-format \"%R\"))" \
    --load "$x_org_capture_el" \
    --eval "(progn
              (let ((capture-buffer (get-buffer-create \"*capture*\"))
                    (journal-buffer (get-buffer-create \"*journal*\"))
                    selected-name
                    current-name
                    line-text)
                (with-current-buffer capture-buffer
                  (erase-buffer)
                  (insert \"capture\"))
                (with-current-buffer journal-buffer
                  (erase-buffer)
                  (org-mode)
                  (insert \"#+TITLE: April 04-12\\n\\n* April 12\\n** 2026 :Sun:\\n*** existing entry\\n\"))
                (switch-to-buffer capture-buffer)
                (cl-letf (((symbol-function 'org-roam-dailies-capture-today)
                           (lambda (&optional _goto _keys)
                             (switch-to-buffer journal-buffer)
                             (goto-char (point-min)))))
                  (x/find-journal-location)
                  (setq selected-name (buffer-name (window-buffer (selected-window))))
                  (setq current-name (buffer-name (current-buffer)))
                  (setq line-text
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
                (princ (format \"selected=%s\\n\" selected-name))
                (princ (format \"current=%s\\n\" current-name))
                (princ (format \"line=%s\\n\" line-text))))"
)"

print -- "$output" | rg -Fx 'selected=*capture*' >/dev/null || {
  print -u2 "expected x/find-journal-location to keep the selected window on the capture buffer"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -Fx 'current=*journal*' >/dev/null || {
  print -u2 "expected x/find-journal-location to hand org-capture the journal buffer"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -Fx 'line=*** existing entry' >/dev/null || {
  print -u2 "expected x/find-journal-location to position point at the first journal subheading"
  print -u2 "$output"
  exit 1
}
