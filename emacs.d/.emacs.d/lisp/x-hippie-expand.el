;;; x-hippie-expand.el --- x-hippie-expand -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-set-key (kbd "M-c") #'hippie-expand)

(provide 'x-hippie-expand)
;;; x-hippie-expand.el ends here
