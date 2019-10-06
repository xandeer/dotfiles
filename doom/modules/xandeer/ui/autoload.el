;;; xandeer/ui/autoload.el -*- lexical-binding: t; -*-

;;;###autodef
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

;;;###autodef
(defun +ui/init-popup-rules ()
  (set-popup-rules!
    '(("^\\*Org Agenda"    :size 0.4 :quit nil :select t :autosave t :modeline t :ttl nil))))

;;;###autodef
(defun +ui/set-font (en cn en-size cn-size)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" en en-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family cn :size cn-size :weight 'bold))))
