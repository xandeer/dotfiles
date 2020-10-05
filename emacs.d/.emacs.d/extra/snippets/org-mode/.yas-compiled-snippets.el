;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("verse" "#+begin_verse\n$0\n#+end_verse" "verse" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/verse" nil nil)
                       ("table" "#+caption: ${1: caption of the table}\n|${2:column 1} | ${3: column 2} |\n|--------------+----------------|" "table" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/table" nil nil)
                       ("src" "#+begin_src ${1:language}\n$0\n#+end_src" "source block" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/src" nil nil)
                       ("name" "#+srcname: $0" "source name" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/sourcename" nil nil)
                       ("sh" "#+begin_src sh\n$0\n#+end_src" "shell" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/shell" nil nil)
                       ("rs" "#+begin_src rust\n$0\n#+end_src\n" "rust" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/rust" nil nil)
                       ("rk" "#+ROAM_KEY: $0" "Roam Key" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/roam-key" nil nil)
                       ("ra" "#+ROAM_ALIAS: $0" "Roam Alias" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/roam-alias" nil nil)
                       ("quote" "#+begin_quote\n$0\n#+end_quote" "quote" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/quote" nil nil)
                       ("-l" "- link :: $0" "- link ::" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/link" nil nil)
                       ("kt" "#+begin_src kotlin\n$0\n#+end_src" "kotlin" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/kotlin" nil nil)
                       ("js" "#+begin_src js\n$0\n#+end_src" "javascript" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/javascript" nil nil)
                       ("-f" "- from :: $0\n" "- from ::" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/from" nil nil)
                       ("example" "#+begin_example\n$0\n#+end_example" "example" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/example" nil nil)
                       ("elisp" "#+begin_src elisp\n$0\n#+end_src" "elisp block" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/elisp" nil nil)
                       ("comment" "#+begin_comment\n$0\n`(%t)`\n#+end_comment" "comment" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/comment" nil nil)
                       ("<v"
                        (progn
                          (%alias "verse"))
                        "#+begin_verse" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/begin_verse" nil nil)
                       ("<s" "#+begin_src ${1:language}\n$0\n#+end_src" "#+begin_src" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/begin_src" nil nil)
                       ("<q"
                        (progn
                          (%alias "quote"))
                        "#+begin_quote" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/begin_quote" nil nil)
                       ("<e"
                        (progn
                          (%alias "example"))
                        "#+begin_example" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/begin_example" nil nil)
                       ("<c"
                        (progn
                          (%alias "comment"))
                        "#+begin_comment" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/begin_comment" nil nil)
                       ("-a" "- author :: $0\n" "- author ::" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/authors" nil nil)
                       ("abr" "**** $1\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:ANKI_TAGS: languages\n:END:\n***** Front\n*$1* $0\n***** Back\n" "Anki Base (and reversed card)" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/anki-base-reverse" nil nil)
                       ("ab" "**** $1\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_TAGS: languages\n:END:\n***** Front\n*$1* $0\n***** Back\n" "Anki Base" nil nil nil "/Users/kevin/.emacs.d/extra/snippets/org-mode/anki-base" nil nil)))


;;; Do not edit! File generated at Sun Oct  4 11:25:21 2020
