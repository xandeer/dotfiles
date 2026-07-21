#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
x_transients_el="$repo_root/emacs.d/.emacs.d/lisp/x-transients.el"

[[ -f "$x_transients_el" ]] || {
  print -u2 "expected repository-managed Emacs transients module at $x_transients_el"
  exit 1
}

output="$(
  emacs --batch -Q \
    --eval "(progn
              (require 'transient)
              (defmacro consult-customize (&rest _args) nil)
              (provide 'consult)
              (defun x/define-keys (&rest _args) nil)
              (defvar test/opened-url nil)
              (defun x/open (url)
                (setq test/opened-url url))
              (defun org-clock-in-last () (interactive))
              (defun org-clock-out () (interactive))
              (defun x/org-done-current () (interactive)))" \
    --load "$x_transients_el" \
    --eval "(dolist (key '(\"i\" \"l\" \"k\"))
              (let* ((suffix (transient-get-suffix
                              'x/transient-global-group key))
                     (properties (nth 2 suffix))
                     (description (plist-get properties :description))
                     (command (plist-get properties :command)))
                (setq test/opened-url nil)
                (call-interactively command)
                (princ (format \"%s|%s|%s\\n\"
                               key description (or test/opened-url \"\")))))"
)"

expected_lines=(
  'i|In last|remspark://trackingAction?action=resume'
  'l|Out|remspark://trackingAction?action=pause'
  'k|Done current|remspark://trackingAction?action=complete'
)

for expected_line in "${expected_lines[@]}"; do
  print -r -- "$output" | rg -Fx -- "$expected_line" >/dev/null || {
    print -u2 "expected runtime RemSpark tracking action: $expected_line"
    print -u2 -- "$output"
    exit 1
  }
done
