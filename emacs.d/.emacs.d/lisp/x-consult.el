;;; x-consult.el --- x-consult -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'consult)
(require 'consult-imenu)

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0
      register-preview-function #'consult-register-format)

;; Optionally tweak the register preview window.
;; This adds thin lines, sorting and hides the mode line of the window.
(advice-add #'register-preview :override #'consult-register-window)

;; Optionally replace `completing-read-multiple' with an enhanced version.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
;; (advice-remove #'completing-read-multiple  #'consult-completing-read-multiple)

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(setq consult-async-min-input 1)
(setq consult-ripgrep-args "rg --null --hidden --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number .")

(autoload 'projectile-project-root "projectile")
(setq consult-project-root-function #'projectile-project-root)

(with-eval-after-load 'orderless
  ;; https://github.com/minad/consult/wiki#use-orderless-as-pattern-compiler-for-consult-grepripgrepfind
  (defun consult--orderless-regexp-compiler (input type ignore-case)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))

  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler))

(defun x/consult-line-dwim ()
  "Call `consult-line' with `(thing-at-point 'symbol)' as initial param."
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;; (global-set-key (kbd "C-c h") 'consult-history)
;; (global-set-key (kbd "C-c m") 'consult-mode-command)
;; (global-set-key (kbd "C-c k") 'consult-kmacro)
(x/define-keys ctl-x-map
               '(([remap switch-to-buffer] consult-buffer)))
(x/define-keys ctl-x-5-map
               '(("b" consult-buffer-other-frame)))
;; Other custom bindings
(x/define-keys global-map
               '(("M-y" consult-yank-pop)
                 ("<help> a" consult-apropos)
                 ("C-s" consult-line)))

(global-set-key (kbd "C-x M-:") 'consult-complex-command) ;; orig. repeat-complex-command

;;; M-s map
(define-prefix-command 'x/meta-s-map)
(global-set-key (kbd "M-s") #'x/meta-s-map)
(x/define-keys
 x/meta-s-map
 '(("l" consult-focus-lines)
   ("s" x/consult-line-dwim)))

(provide 'x-consult)
;;; x-consult.el ends here
