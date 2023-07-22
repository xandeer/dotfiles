;;; x-search-engine.el --- x-search-engine -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/append-init-hook (lambda () (engine-mode t)))

(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
(setq browse-url-generic-program (executable-find "open"))
;; (setq engine/browser-function 'browse-url-generic)
(setq engine/browser-function 'xwidget-webkit-browse-url)

(defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "u")
(defengine google "http://www.google.com/search?q=%s" :keybinding "g")
(defengine grep-app "http://grep.app/search?q=%s" :keybinding "c")
(defengine wiki-en "https://en.wikipedia.org/wiki/%s" :keybinding "w")
(defengine wiki-cn "https://zh.wikipedia.org/wiki/%s" :keybinding "f")
(defengine zlib (concat x/zlib-domain "/s/%s?extensions[]=epub") :keybinding "b")

(provide 'x-search-engine)
;;; x-search-engine.el ends here
