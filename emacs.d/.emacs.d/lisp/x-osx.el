;;; x-osx.el --- x-osx -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
(setq mac-right-command-modifier 'hyper)
(setq mac-function-modifier 'super)

(x/define-keys
 global-map
 '(("H-c" kill-ring-save)
   ("H-v" yank)))

(setq locate-command "mdfind")

(setq mac-redisplay-dont-reset-vscroll t
      mac-mouse-wheel-smooth-scroll nil)

(setq delete-by-moving-to-trash t)
(autoload #'osx-trash-move-file-to-trash "osx-trash" nil t)

(unless (fboundp 'system-move-file-to-trash)
  (defalias #'system-move-file-to-trash #'osx-trash-move-file-to-trash))

(provide 'x-osx)
;;; x-osx.el ends here
