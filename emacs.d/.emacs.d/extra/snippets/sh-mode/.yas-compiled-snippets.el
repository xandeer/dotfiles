;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
                     '(("if" "if ${1:[ -f file]}; then\n  ${2:do}\nfi\n$0" "if" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/sh-mode/if" nil nil)
                       ("f" "function ${1:name} {\n  $0\n}\n" "function" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/sh-mode/function" nil nil)
                       ("for" "for ${1:var} in ${2:stuff}; do\n  $0\ndone" "for loop" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/sh-mode/for" nil nil)
                       ("!" "#!/usr/bin/env bash\n$0" "bang" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/sh-mode/bang" nil nil)))


;;; Do not edit! File generated at Sun Oct  4 11:25:21 2020
