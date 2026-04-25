#!/bin/zsh

set -eu

repo_root="${0:A:h:h:h}"
x_browser_el="$repo_root/emacs.d/.emacs.d/lisp/x-browser.el"

[[ -f "$x_browser_el" ]] || {
  print -u2 "expected repository-managed Emacs browser module at $x_browser_el"
  exit 1
}

output="$(
  emacs --batch -Q \
    --eval "(progn
              (require 'bookmark)
              (require 'browse-url)
              (defvar test/browse-calls nil)
              (defun test/browse-capture (url &rest _args)
                (push url test/browse-calls))
              (defmacro x/package-use (&rest _) nil)
              (defun x/expand-note (path) path)
              (defun ebuku-update-bookmarks-cache () nil)
              (setq browse-url-secondary-browser-function #'test/browse-capture)
              (setq browse-url-browser-function #'test/browse-capture)
              (setq ebuku-bookmarks
                    '(((title . \"Example\")
                       (url . \"https://example.com\")
                       (index . \"1\")))))" \
    --load "$x_browser_el" \
    --eval "(progn
              (x/update-bookmarks)
              (setq bookmark-alist x/bookmarks)
              (condition-case err
                  (progn
                    (setq test/browse-calls nil)
                    (x/open-bookmark \"Example\")
                    (princ (format \"direct=%s\\n\" (car test/browse-calls))))
                (error
                 (princ (format \"direct-error=%S\\n\" err))))
              (condition-case err
                  (progn
                    (setq test/browse-calls nil)
                    (bookmark-jump \"Example\")
                    (princ (format \"bookmark=%s\\n\" (car test/browse-calls))))
                (error
                 (princ (format \"bookmark-error=%S\\n\" err)))))"
)"

print -- "$output" | rg -Fx 'direct=https://example.com' >/dev/null || {
  print -u2 "expected x/open-bookmark to open buku URLs in the external browser without preloading eww"
  print -u2 "$output"
  exit 1
}

print -- "$output" | rg -Fx 'bookmark=https://example.com' >/dev/null || {
  print -u2 "expected bookmark handlers for buku URLs to pass the URL string to the external browser"
  print -u2 "$output"
  exit 1
}
