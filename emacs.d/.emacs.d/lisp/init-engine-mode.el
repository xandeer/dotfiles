;;; init-engine-mode.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(engine-mode
   :host github
   :repo "hrs/engine-mode"
   :branch "main"))
(straight-use-package 'engine-mode)

(leaf engine-mode
  :config
  (engine-mode t)
  (setq browse-url-generic-program (executable-find "open"))
  (setq engine/browser-function 'browse-url-generic)
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "u")
  (defengine google
    "http://www.google.com/search?q=%s"
    :keybinding "g")
  (defengine grep-app
    "http://grep.app/search?q=%s"
    :keybinding "c")
  (defengine translate-cn
    "https://translate.google.com/?sl=auto&tl=zh-CN&text=%s"
    :keybinding "a")
  (defengine translate-en
    "https://translate.google.com/?sl=auto&tl=en&text=%s"
    :keybinding "b")
  (defengine wiki-en
    "https://en.wikipedia.org/wiki/%s"
    :keybinding "w")
  (defengine wiki-cn
    "https://zh.wikipedia.org/wiki/%s"
    :keybinding "f"))

(provide 'init-engine-mode)
;;; init-engine-mode.el ends here
