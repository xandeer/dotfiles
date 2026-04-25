#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
x_org_capture_el="$repo_root/emacs.d/.emacs.d/lisp/x-org-capture.el"
journal_file="$(mktemp /tmp/org-capture-journal-cli.XXXXXX.org)"

trap 'rm -f "$journal_file"' EXIT

[[ -f "$x_org_capture_el" ]] || {
  print -u2 "expected repository-managed Emacs org-capture module at $x_org_capture_el"
  exit 1
}

cat > "$journal_file" <<'EOF'
#+TITLE: April 04-12

* April 12
** 2026 :Sun:
*** existing entry
EOF

output="$(
  emacs --batch -Q \
    --eval "(progn
              (require 'org-capture)
              (require 'org)
              (defun x/define-keys (&rest _args) nil)
              (defun x/insert-weather () nil)
              (defvar x/org-today-tag \"today\"))" \
    --load "$x_org_capture_el" \
    --eval "(progn
              (let ((journal-buffer (find-file-noselect \"$journal_file\"))
                    result
                    selected-name
                    current-name)
                (with-current-buffer journal-buffer
                  (erase-buffer)
                  (org-mode)
                  (insert \"#+TITLE: April 04-12\\n\\n* April 12\\n** 2026 :Sun:\\n*** existing entry\\n\"))
                (switch-to-buffer (get-buffer-create \"*scratch*\"))
                (cl-letf (((symbol-function 'org-roam-dailies-capture-today)
                           (lambda (&optional _goto _keys)
                             (switch-to-buffer journal-buffer)
                             (goto-char (point-min)))))
                  (condition-case err
                      (progn
                        (x/journal-capture-string \"CLI text\")
                        (setq selected-name (buffer-name (window-buffer (selected-window))))
                        (setq current-name (buffer-name (current-buffer)))
                        (setq result (with-current-buffer journal-buffer
                                       (buffer-string)))
                        (princ (format \"selected=%s\\n\" selected-name))
                        (princ (format \"current=%s\\n\" current-name))
                        (princ result))
                    (error
                     (princ (format \"error=%S\\n\" err)))))))"
)"

print -- "$output" | rg -Fx 'selected=*scratch*' >/dev/null || {
  print -u2 "expected CLI journal capture to return focus to the original buffer after immediate finish"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -Fx 'current=*scratch*' >/dev/null || {
  print -u2 "expected CLI journal capture to leave the current buffer on the original buffer after immediate finish"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -e '^\*\*\* <[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}> CLI text$' >/dev/null || {
  print -u2 "expected direct CLI journal capture to create an inline timestamped journal entry"
  print -u2 "$output"
  exit 1
}
