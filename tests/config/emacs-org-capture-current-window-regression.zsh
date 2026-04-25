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
              (defun x/define-keys (&rest _args) nil)
              (defun x/insert-weather () nil)
              (defvar x/org-today-tag \"today\")
              (defvar org-journal-time-format \"%R\"))" \
    --load "$x_org_capture_el" \
    --eval "(progn
              (delete-other-windows)
              (let* ((left (selected-window))
                     (right (split-window-right))
                     (left-buffer (get-buffer-create \"*left*\"))
                     (right-buffer (get-buffer-create \"*right*\"))
                     (capture-buffer (get-buffer-create \"*Capture*\")))
                (set-window-buffer left left-buffer)
                (set-window-buffer right right-buffer)
                (select-window right)
                (x/org-capture-place-template-current-window
                 (lambda (&rest _args)
                   (pop-to-buffer capture-buffer '(org-display-buffer-split))))
                (princ (format \"selected=%s\\n\" (buffer-name (window-buffer (selected-window)))))
                (princ (format \"left=%s\\n\" (buffer-name (window-buffer left))))
                (princ (format \"right=%s\\n\" (buffer-name (window-buffer right))))))"
)"

print -- "$output" | rg -Fx 'selected=*Capture*' >/dev/null || {
  print -u2 "expected capture to remain selected after opening the capture buffer"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -Fx 'left=*left*' >/dev/null || {
  print -u2 "expected the left window to keep its original buffer when capture starts from the right window"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -Fx 'right=*Capture*' >/dev/null || {
  print -u2 "expected the capture buffer to open in the selected right window"
  print -u2 "$output"
  exit 1
}
