;;; x-cloel.el --- cloel -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use 'parseedn)
(x/package-use '(cloel . "manateelazycat/cloel"))

(require 'parseedn)
(require 'cloel)


;; (x/start-process (format "cd %s && echo pwd && clojure -X:jar&& clojure -X:install" (straight--repos-dir "cloel")))


(provide 'x-cloel)
;;; x-cloel.el ends here
