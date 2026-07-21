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
              (defvar test/process-argv nil)
              (defun x/open (&rest _args)
                (error \"x/open must not handle RemSpark tracking actions\"))
              (defun x/start-process (cmd &optional _switch _sentinel)
                (setq test/process-argv (split-string cmd)))
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
                (setq test/process-argv nil)
                (call-interactively command)
                (princ (format \"%s|%s|%d|%s|%s\\n\"
                               key description
                               (length test/process-argv)
                               (or (nth 0 test/process-argv) \"\")
                               (or (nth 1 test/process-argv) \"\")))))"
)"

expected_lines=(
  'i|In last|2|/usr/bin/open|remspark://trackingAction?action=resume'
  'l|Out|2|/usr/bin/open|remspark://trackingAction?action=pause'
  'k|Done current|2|/usr/bin/open|remspark://trackingAction?action=complete'
)

for expected_line in "${expected_lines[@]}"; do
  print -r -- "$output" | rg -Fx -- "$expected_line" >/dev/null || {
    print -u2 "expected runtime RemSpark tracking action argv: $expected_line"
    print -u2 -- "$output"
    exit 1
  }
done
