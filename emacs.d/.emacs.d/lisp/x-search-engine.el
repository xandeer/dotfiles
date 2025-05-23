;;; x-search-engine.el --- x-search-engine -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/append-init-hook (lambda () (engine-mode t)))

(setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox")
(setq browse-url-generic-program (executable-find "open"))
(setq engine/browser-function 'browse-url-generic)

(defengine google "http://www.google.com/search?q=%s" :keybinding "g")
(defengine grep-app "http://grep.app/search?q=%s" :keybinding "c")
;; search field doesn't work
(defengine deepwiki "https://deepwiki.com?search_repo=%s" :keybinding "d")
(defengine zlib (concat x/zlib-domain "/s/%s?extensions[]=epub") :keybinding "b")

(defengine wiki-en "https://en.wikipedia.org/wiki/%s" :keybinding "w" :browser 'eww)
(defengine wiki-cn "https://zh.wikipedia.org/wiki/%s" :keybinding "f")

(defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "u")
(defengine perplexity "https://www.perplexity.ai/search?q=%s" :keybinding "a")

(provide 'x-search-engine)
;;; x-search-engine.el ends here
