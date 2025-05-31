;;; x-omni.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use '(consult-omni . ("armindarvish/consult-omni" :files (:defaults "sources/*.el"))))
(require 'consult)
(require 'consult-omni-sources)
;; Load Embark Actions
(require 'consult-omni-embark)
;; Only load brave-auto-suggest source
(require 'consult-omni-brave-autosuggest)
;;; Set your shorthand favorite interactive command
(setq consult-omni-default-interactive-command #'consult-omni-brave-autosuggest)

(setq consult-omni-multi-sources '("calc"
                                   ;; "File"
                                   "Buffer"
                                   ;; "Bookmark"
                                   "Apps"
                                   ;; "gptel"
                                   "Brave"
                                   "Dictionary"
                                   ;; "Google"
                                   "Wikipedia"
                                   "elfeed"
                                   ;; "mu4e"
                                   ;; "buffers text search"
                                   "Notes Search"
                                   "Org Agenda"
                                   "GitHub"
                                   ;; "YouTube"
                                   "Invidious"))
(setq consult-omni-sources-modules-to-load '(consult-omni-google consult-omni-wikipedia consult-omni-buffer consult-omni-apps consult-omni-bookmark))
(consult-omni-sources-load-modules)

(provide 'x-omni)
;;; x-omni.el ends here
