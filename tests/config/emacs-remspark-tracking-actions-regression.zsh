#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
x_start_process_el="$repo_root/emacs.d/.emacs.d/lisp/x-start-process.el"
x_transients_el="$repo_root/emacs.d/.emacs.d/lisp/x-transients.el"

[[ -f "$x_start_process_el" ]] || {
  print -u2 "expected repository-managed process helper at $x_start_process_el"
  exit 1
}

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
              (defvar test/process-commands nil)
              (defun s-starts-with? (prefix string)
                (string-prefix-p prefix string))
              (defun make-process (&rest args)
                (push (plist-get args :command) test/process-commands)
                'test/process)
              (defun set-process-sentinel (&rest _args) nil)
              (defun x/open (&rest _args)
                (error \"x/open must not handle RemSpark tracking actions\"))
              (defun org-clock-in-last () (interactive))
              (defun org-clock-out () (interactive))
              (defun x/org-done-current () (interactive)))" \
    --load "$x_start_process_el" \
    --load "$x_transients_el" \
    --eval "(progn
              (dolist (key '(\"i\" \"l\" \"k\"))
                (let* ((suffix (transient-get-suffix
                                'x/transient-global-group key))
                       (properties (nth 2 suffix))
                       (description (plist-get properties :description))
                       (command (plist-get properties :command)))
                  (call-interactively command)
                  (let ((argv (car test/process-commands)))
                    (princ (format \"%s|%s|%d|%s|%s\\n\"
                                   key description
                                   (length argv)
                                   (or (nth 0 argv) \"\")
                                   (or (nth 1 argv) \"\"))))))
              (princ (format \"dispatch-count|%d\\n\"
                             (length test/process-commands))))"
)"

expected_lines=(
  'i|In last|2|/usr/bin/open|remspark://trackingAction?action=resume'
  'l|Out|2|/usr/bin/open|remspark://trackingAction?action=pause'
  'k|Done current|2|/usr/bin/open|remspark://trackingAction?action=complete'
  'dispatch-count|3'
)

for expected_line in "${expected_lines[@]}"; do
  print -r -- "$output" | rg -Fx -- "$expected_line" >/dev/null || {
    print -u2 "expected runtime RemSpark tracking action argv: $expected_line"
    print -u2 -- "$output"
    exit 1
  }
done
