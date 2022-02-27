;;; x-consult.el --- x-consult -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'consult t)
(require-package 'wgrep)
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

;; Use Consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(with-eval-after-load 'consult
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

  (defun xr/search-in-lisp ()
    "Start searching in `user-emacs-directory`."
    (interactive)
    (consult-ripgrep
      (expand-file-name "lisp" user-emacs-directory)))

  (global-set-key (kbd "C-c h") 'consult-history)
  (global-set-key (kbd "C-c m") 'consult-mode-command)
  (global-set-key (kbd "C-c k") 'consult-kmacro)
  (global-set-key (kbd "C-c f e") 'xr/search-in-lisp)
  ;; C-x bindings (ctl-x-map)
  (global-set-key (kbd "C-x M-:") 'consult-complex-command)     ;; orig. repeat-complex-command
  (global-set-key (kbd "C-x b") 'consult-buffer)                ;; orig. switch-to-buffer
  (global-set-key (kbd "C-x C-b") 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  (global-set-key (kbd "C-x 5 b") 'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;; Custom M-# bindings for fast register access
  (global-set-key (kbd "M-#") 'consult-register-load)
  (global-set-key (kbd "M-'") 'consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  (global-set-key (kbd "C-M-#") 'consult-register)
  ;; Other custom bindings
  (global-set-key (kbd "M-y") 'consult-yank-pop)                ;; orig. yank-pop
  (global-set-key (kbd "<help> a") 'consult-apropos)            ;; orig. apropos-command
  ;; M-g bindings (goto-map)
  ;; (global-set-key (kbd "M-g e") 'consult-compile-error)
  ;; (global-set-key (kbd "M-g f") 'consult-flymake)               ;; Alternative: consult-flycheck
  (global-set-key (kbd "M-g g") 'consult-goto-line)             ;; orig. goto-line
  (global-set-key (kbd "M-g M-g") 'consult-goto-line)           ;; orig. goto-line
  (global-set-key (kbd "M-g h") 'consult-outline)               ;; Alternative: consult-org-heading
  (global-set-key (kbd "M-g m") 'consult-mark)
  (global-set-key (kbd "M-g k") 'consult-global-mark)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g I") 'consult-imenu-multi)
  ;; M-s bindings (search-map)
  (global-set-key (kbd "M-s f") 'consult-find)
  (global-set-key (kbd "M-s F") 'consult-locate)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s G") 'consult-git-grep)
  (global-set-key (kbd "M-s s") (lambda () (interactive) (consult-ripgrep default-directory)))
  (global-set-key (kbd "M-s l") 'consult-line)
  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "M-s L") 'consult-line-multi)
  (global-set-key (kbd "M-s m") 'consult-multi-occur)
  (global-set-key (kbd "M-s k") 'consult-keep-lines)
  (global-set-key (kbd "M-s u") 'consult-focus-lines)
  ;; Isearch integration
  (global-set-key (kbd "M-s e") 'consult-isearch-history)
  ;; isearch-mode-map
  (define-key isearch-mode-map (kbd "M-e") 'consult-isearch-history)         ;; orig. isearch-edit-string
  (define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history)       ;; orig. isearch-edit-string
  (define-key isearch-mode-map (kbd "M-s l") 'consult-line)                  ;; needed by consult-line to detect isearch
  (define-key isearch-mode-map (kbd "M-s L") 'consult-line-multi)           ;; needed by consult-line to detect isearch
  )

(provide 'x-consult)
;;; x-consult.el ends here
