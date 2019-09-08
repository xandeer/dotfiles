;;; xandeer/ui/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ui/set-org-pretty-symbols ()
  (set-pretty-symbols! 'org-mode
    ;; original
    :name "#+NAME:"
    :src_block "#+BEGIN_SRC"
    :src_block_end "#+END_SRC"
    ;; customized
    :alist '(
             ("#+BEGIN_VERSE" . "☘")
             ("#+END_VERSE" . "☘")
             ("#+BEGIN_QUOTE" . "⚶")
             ("#+END_QUOTE" . "⚶")
             ("#+BEGIN_EXAMPLE" . "♒")
             ("#+END_EXAMPLE" . "♒")
             ("#+BEGIN_COMMENT" . "☕")
             ("#+END_COMMENT" . "☕")
             )
    :merge t))
