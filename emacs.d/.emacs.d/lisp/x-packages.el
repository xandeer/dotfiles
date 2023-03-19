;;; x-packages.el --- x-packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; libs
(defun x/package-use (package)
  "`straight-use-package' wrapper.

Usages:
  (x/package-use 'anzu)
  (x/package-use '(org . (:type built-in)))
  (x/package-use '(consult . \"minad/consult\"))
  (x/package-use '(copilot . (\"zerolfx/copilot.el\" :files (\"*.el\" \"dist\")))"
  (let ((plist `(,(or (and (listp package)
                           (car package))
                      package))))
    (when (listp package)
      (let ((repo (or (and (stringp (cdr package))
                           (cdr package))
                      (and (listp (cdr package))
                           (not (plistp (cdr package)))
                           (cadr package))))
            (host (if (and (listp (cdr package))
                           (plistp (cddr package))
			   (plist-member (cddr package) :host))
		      (plist-get (cddr package) :host)
                      'github))
            (files (and (listp (cdr package))
                        (plistp (cddr package))
                        (plist-get (cddr package) :files)))
            (type (and (plistp (cdr package))
                       (plist-get (cdr package) :type))))
        (when repo
          (setq plist `(,@plist :host ,host :repo ,repo)))
        (when files
          (setq plist `(,@plist :files ,files)))
        (when type
          (setq plist `(,@plist :type ,type)))))

    (straight-use-package `,plist)))

;;; pinned
;; Save versions without pinned by `straight-x-freeze-versions'.
;; Save pinned by `straight-x-freeze-pinned-versions'.
;; Sync all by `straight-x-pull-all'.
;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      `((nil . ,(expand-file-name "etc/packages-default.el" vanilla-path))
        ;; Packages which are pinned to a specific commit.
        (pinned . ,(expand-file-name "etc/packages-pinned.el" vanilla-path))))

;; Sync just pinned by `straight-x-thaw-pinned-versions'.
(defvar x/packages-pinned
  '((consult . "minad/consult")
    telega)
  "Pinned packages install with `x/package-use'")

(setq straight-x-pinned-packages
      '(("consult" . "f517b70dd8a3be0b8c883633f2a7721448b40f0f")
        ("telega.el" . "11c0c785ed9e479b65514a10022d5c34984aab59")))

;;; others
(defvar x/packages
  '(
;;; core
    ;; copied a counterpart in other folder
    ;; (short-lambda . "abo-abo/short-lambda")
;;; basic
    all-the-icons
    anzu
    avy
    (consult . "minad/consult")
    ;; todo: for what?
    explain-pause-mode
    gcmh
    (hydra . "abo-abo/hydra")
    no-littering
    ;; pcache
    session
    wgrep

;;; editor
    (auto-save . "manateelazycat/auto-save")
    color-identifiers-mode
    easy-kill
    eldoc-box
    expand-region
    highlight-indent-guides
    htmlize
    hungry-delete
    indent-tools
    mmm-mode
    page-break-lines
    quick-peek
    rainbow-delimiters
    rainbow-identifiers
    rainbow-mode
    smartparens
    titlecase
    unfill
    visual-fill-column

;;; ui
    doom-themes
    doom-modeline
    default-text-scale
    orderless
    vertico
    (vertico-posframe . "tumashu/vertico-posframe")

;;; window
    ace-window
    golden-ratio

;;; completion
    (cape  . "minad/cape")
    company
    (copilot . ("zerolfx/copilot.el" :files ("*.el" "dist")))
    corfu
    (corfu-doc . "galeo/corfu-doc")
    ;; (kind-icon . "jdtsmith/kind-icon")
    ;; required by hind-icon
    ;; (svg-lib . "rougier/svg-lib")
    kind-icon
    lsp-mode
    lsp-ui
    svg-lib
    (tempel . "minad/tempel")
    yasnippet

;;; org
    (org . (:type built-in))
    ob-restclient
    (org-modern . "minad/org-modern")
    org-roam
    org-transclusion
    consult-org-roam
    ;; used for roam
    emacsql-sqlite-builtin
    restclient

;;; programs
    devdocs
    consult-dash
    dash-docs
    flycheck

    ;; clojure
    clojure-mode
    cider
    flycheck-clj-kondo
    parseclj

    ;; elisp
    elisp-def
    eros
    helpful
    lispy
    multiple-cursors

    ;; elixir
    elixir-mode
    alchemist
    exunit
    flycheck-credo

    ;; json
    json-mode
    json-reformat
    ;; jq-format

    ;; kotlin
    kotlin-mode
    flycheck-kotlin
    (ob-kotlin . "zweifisch/ob-kotlin")

    ;; web
    ;; company-web
    ob-typescript
    skewer-mode
    typescript-mode
    tide
    ts-comint
    web-mode
    (yarn . "jmfirth/yarn.el")

;;; git
    abridge-diff
    diff-hl
    forge
    git-blamed
    (git-gutter-fringe . "emacsorphanage/git-gutter-fringe")
    git-link
    git-messenger
    git-modes
    (git-timemachine . ("https://codeberg.org/pidu/git-timemachine.git" :host nil))
    magit

;;; keybindings
    command-log-mode
    keyfreq
    which-key

;;; tools
    ace-pinyin
    cal-china-x
    ebuku
    embark
    embark-consult
    (eva . ("meedstrom/eva" :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))
    dired-hacks
    dirvish
    go-translate
    (jieba . ("cireu/jieba.el" :files ("*.el" "*.json" "*.js")))
    link-hint
    makefile-executor
    marginalia
    meow
    (nov . ("https://depp.brause.cc/nov.el.git" :host nil))
    org-anki
    password-store
    (pinyinlib . "xlshiz/pinyinlib.el")
    projectile
    posframe
    (rime . ("DogLooksGood/emacs-rime" :files ("Makefile" "*.el" "lib.c")))
    (sdcv . "manateelazycat/sdcv")
    shrface
    shr-tag-pre-highlight
    wakatime-mode

;;; shell
    eshell-z

;;; browser
    (engine-mode . "hrs/engine-mode")
    ;; required by xwwp
    ctable
    (xwwp . ("xandeer/xwwp" :files (:defaults "*.js" "*.css")))

;;; osx
    osx-lib
    osx-trash)
  "List of packages to install with `x/package-use'.")

;;; install
(with-eval-after-load 'straight
  (require 'straight-x)

  (let ((straight-current-profile 'pinned))
    (mapc #'x/package-use x/packages-pinned))

  (mapc #'x/package-use x/packages))

(provide 'x-packages)
;;; x-packages.el ends here
