;; -*- no-byte-compile: t; -*-
;;; xandeer/chinese/packages.el

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
