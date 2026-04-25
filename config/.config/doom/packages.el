;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! telega
  :recipe (:host github
           :repo "zevlg/telega.el"
           :branch "master")
  :pin "1080f10d1f87ea8f096ff451be831b953e4ccb98")

(package! consult
  :recipe (:host github
           :repo "minad/consult")
  ;; tag 0.17
  :pin "f517b70dd8a3be0b8c883633f2a7721448b40f0f")


(package! abridge-diff)
(package! ace-pinyin)
(package! ace-window)
(package! auto-save
  :recipe (:host github
           :repo "manateelazycat/auto-save"))
(package! cal-china-x)
(package! cape
  :recipe (:host github
           :repo "minad/cape"))
(package! color-identifiers-mode)
(package! command-log-mode)
(package! company-web)
(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))
(package! corfu)
(package! corfu-doc
  :recipe (:host github
           :repo "galeo/corfu-doc"))
(package! ctable)
(package! default-text-scale)
(package! devdocs)
(package! dired-hacks)
(package! easy-kill)
(package! ebuku)
(package! engine-mode)
(package! eva
  :recipe (:host github
           :repo "meedstrom/eva"
           :branch "master"
           :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))
(package! git-blamed)
(package! git-messenger)
(package! go-translate)
(package! golden-ratio)
(package! highlight-indent-guides)
(package! htmlize)
(package! hungry-delete)
(package! iedit)
(package! indent-tools)
(package! jieba
  :recipe (:host github
           :repo "cireu/jieba.el"
           :files ("*.el" "*.json" "*.js")))
(package! jq-format)
(package! json-mode)
(package! json-reformat)
(package! json-snatcher)
(package! keyfreq)
(package! kind-icon)
(package! lispy)
(package! loop)
(package! lsp-mode)
(package! lsp-ui)
(package! meow)
(package! mmm-mode)
(package! named-timer)
(package! no-littering)
(package! ob-kotlin)
(package! ob-restclient)
(package! org
  :recipe (:type built-in))
(package! org-modern)
(package! org-roam)
(package! osx-lib)
(package! page-break-lines)
(package! posframe)
(package! pos-tip)
(package! quick-peek)
(package! rainbow-identifiers)
(package! reformatter)
(package! restclient)
(package! rime
  :recipe (:host github
           :repo "DogLooksGood/emacs-rime"
           :files ("Makefile" "*.el" "lib.c")))
(package! sdcv
  :recipe (:host github
           :repo "manateelazycat/sdcv"))
(package! session)
(package! showtip)
(package! suggest)
(package! svg-lib)
(package! swiper)
(package! tempel)
(package! ts)
(package! unfill)
(package! vertico-posframe)
(package! visual-fill-column)
(package! wakatime-mode)
(package! web-completion-data)
(package! xwwp
  :recipe (:host github
           :repo "xandeer/xwwp"
           :files (:defaults "*.js" "*.css")))
(package! yafolding)
(package! yarn
  :recipe (:host github
           :repo "jmfirth/yarn.el"))
(package! zoutline)

(package! ws-butler :disable t)
