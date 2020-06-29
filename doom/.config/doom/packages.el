(package! disable-mouse)

(package! keyfreq)

(package! youdao-dictionary)

(package! deft)

(package! pyim
  :recipe
  (:host github :repo "tumashu/pyim"
       :files (:defaults "*.el")))
(package! posframe)

;; (package! pyim-basedict
;;   :recipe
;;   (:host github :repo "tumashu/pyim-basedict"
;;          :files (:defaults "pyim-basedict.el" "pyim-basedict.pyim")))

(package! pyim-greatdict
  :recipe
  (:host github :repo "tumashu/pyim-greatdict"
       :files (:defaults "pyim-greatdict.el" "pyim-greatdict.pyim.gz")))

(package! cal-china-x
  :recipe
  (:host github :repo "xwl/cal-china-x"
       :files (:defaults "*.el")))

(package! yasnippet)

(package! nord-theme)

(package! ivy-posframe
  :disable t
  :recipe
  (:host github :repo "tumashu/ivy-posframe"
       :files (:defaults "*.el")))

(package! telega
  :recipe
  (:host github
   :repo "zevlg/telega.el"
   :branch "telega-tdlib-150"
   :files (:defaults "README.md" "etc" "server" "Makefile" "test.el")))

(package! beancount
  :disable t
  :recipe
  (:host github :repo "beancount/beancount"
       :files ("editors/emacs/beancount.el")))

(package! anki-editor
  :recipe
  (:host github
   :repo "louietan/anki-editor"
   :branch "master"
   :files (:defaults "*.el")))
