;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("verse" "#+begin_verse\n$0\n`(%t)`\n#+end_verse" "verse" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/verse" nil nil)
                       ("table" "#+CAPTION: ${1: caption of the table}\n|${2:column 1} | ${3: column 2} |\n|--------------+----------------|" "table" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/table" nil nil)
                       ("src" "#+begin_src ${1:language}\n$0\n#+end_src" "source block" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/src" nil nil)
                       ("name" "#+srcname: $0" "source name" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/sourcename" nil nil)
                       ("sh" "#+begin_src sh\n$0\n#+end_src" "shell" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/shell" nil nil)
                       ("quote" "#+begin_quote\n$0\n`(%t)`\n#+end_quote" "quote" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/quote" nil nil)
                       ("kt" "#+begin_src kotlin\n$0\n#+end_src" "kotlin" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/kotlin" nil nil)
                       ("js" "#+begin_src js\n$0\n#+end_src" "javascript" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/javascript" nil nil)
                       ("example" "#+begin_example\n$0\n`(%t)`\n#+end_example" "example" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/example" nil nil)
                       ("elisp" "#+begin_src elisp :tangle yes\n$0\n#+end_src" "elisp block" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/elisp" nil nil)
                       ("comment" "#+begin_comment\n$0\n`(%t)`\n#+end_comment" "comment" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/comment" nil nil)
                       ("<v"
                        (progn
                          (%alias "verse"))
                        "#+begin_verse" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/begin_verse" nil nil)
                       ("<s" "#+begin_src ${1:language}\n$0\n#+end_src" "#+begin_src" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/begin_src" nil nil)
                       ("<q"
                        (progn
                          (%alias "quote"))
                        "#+begin_quote" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/begin_quote" nil nil)
                       ("<e"
                        (progn
                          (%alias "example"))
                        "#+begin_example" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/begin_example" nil nil)
                       ("<c"
                        (progn
                          (%alias "comment"))
                        "#+begin_comment" nil nil nil "/Users/kevin/.config/doom/snippets/org-mode/begin_comment" nil nil)))


;;; Do not edit! File generated at Sat Jul  6 16:51:36 2019
