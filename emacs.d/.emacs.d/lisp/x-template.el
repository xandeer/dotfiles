;;; x-template.el --- x-template -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'tempel)

;;; config
(setq tempel-path (expand-file-name "etc/templates" vanilla-path))

(defun x-template--setup-capf ()
  "Add the Tempel Capf to `completion-at-point-functions".
  (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))

;; (add-hook 'prog-mode-hook #'x-template--setup-capf)
;; (add-hook 'text-mode-hook #'x-template--setup-capf)

;; `expand-abbrev` is bound to "C-x '".
;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
(global-tempel-abbrev-mode)

;;; key bindings
(define-key tempel-map (kbd "TAB") #'tempel-next)
(define-key tempel-map (kbd "C-<tab>") #'tempel-previous)
(define-key tempel-map (kbd "C-g") #'tempel-done)
(define-key tempel-map (kbd "C-c") #'tempel-abort)
(define-key tempel-map (kbd "M-a") #'tempel-beginning)
(define-key tempel-map (kbd "M-e") #'tempel-end)
(global-set-key (kbd "M-C") #'tempel-complete)

;;; org
(defhydra x/hydra-template-org
  (:exit t :columns 4 :idle 0.3)
  "
Org Templates
"
  (";" (tempel-insert 'comment) "comment")
  ("q" (tempel-insert 'quote) "quote")
  ("v" (tempel-insert 'verse) "verse")
  ("w" x/wrap-block "wrap block")
  ("e" (tempel-insert 'elisp) "elisp")
  ("s" (tempel-insert 'sh) "sh")
  ("M-s" (tempel-insert 'src) "src")
  ("M-t" tempel-insert "tempel insert")
  ("k" (tempel-insert 'kotlin) "kotlin")
  ("c" (tempel-insert 'clojure) "clojure")
  ("r" (tempel-insert 'weekly) "weekly review")
  ("m" (tempel-insert 'monthly) "monthly review")
  ("t" (tempel-insert 'time) "timestamp")
  ("d" (tempel-insert 'day) "day: 14(Fri)"))

(define-key org-mode-map (kbd "M-t") #'x/hydra-template-org/body)

;;; elisp
(defhydra x/hydra-template-elisp (:exit t :columns 4 :idle 0.3)
  "
Elisp Templates
"
  ("d" (tempel-insert 'file-template) "elisp header and footer")
  ("h" (tempel-insert 'hydra) "defhydra")
  ("l" (tempel-insert 'lambda) "lambda")
  ("f" (tempel-insert 'fun) "defun")
  ("c" (tempel-insert 'command) "command")
  ("M-l" (tempel-insert 'let) "let")
  ("v" (tempel-insert 'var) "defvar")
  ("M-c" (tempel-insert 'const) "defconst")
  ("S-c" (tempel-insert 'custom) "defcustom"))

(define-key emacs-lisp-mode-map (kbd "M-t") #'x/hydra-template-elisp/body)

;;; global
(defhydra x/hydra-template-global (:exit t :columns 4 :idle 0.3)
  "
Global Templates
"
  ("d" (tempel-insert 'chglog) "git changelog")
  ("v" (tempel-insert 'version) "git app version")
  ("t" (tempel-insert 'time) "timestamp")
  ("M-t" tempel-insert "temple insert"))
(global-set-key (kbd "M-t") #'x/hydra-template-global/body)

;;; file templates
;; Copied from doom
(defun x/template--file-templates-in-emacs-dirs-p (file)
  "Returns t if FILE is in your emacs.d directory."
  (file-in-directory-p file vanilla-path))

(defvar x/template--file-templates-alist
  ;; elisp
  '(("\\.el$" ;; :when x/template--file-templates-in-emacs-dirs-p
     :mode emacs-lisp-mode
     :template file-template)
    ("\\.html$"))
  "An alist of file template rules. The CAR of each rule is either a major mode
symbol or regexp string. The CDR is a plist.")

(defun x/template--file-template-p (rule)
  "Return t if RULE applies to the current buffer."
  (let ((pred (car rule))
        (plist (cdr rule)))
    (and (or (and (symbolp pred)
                  (eq major-mode pred))
             (and (stringp pred)
                  (stringp buffer-file-name)
                  (string-match-p pred buffer-file-name)))
         (or (not (plist-member plist :when))
             (and (funcall (plist-get plist :when)
                           buffer-file-name)))
         rule)))

(defun x/template--file-templates-check-h (&optional args)
  "Check if the current buffer is a candidate for file template expansion. It
must be non-read-only, empty, and there must be a rule in
`x/template--file-templates-alist' that applies to it."
  (and buffer-file-name
       (not buffer-read-only)
       (bobp) (eobp)
       (not (member (substring (buffer-name) 0 1) '("*" " ")))
       (not (file-exists-p buffer-file-name))
       (not (buffer-modified-p))
       (null (buffer-base-buffer))
       (when-let (rule (cl-find-if #'x/template--file-template-p x/template--file-templates-alist))
         (let ((template (or (plist-get (cdr rule) :template)
                             'file-template)))
           (message "Applying %s template to %s" template buffer-file-name)
           (run-with-idle-timer 0.1 nil
                                (lambda ()
                                  (when (functionp 'meow-insert-mode)
                                    (meow-insert-mode))
                                  (tempel-insert template)))))))

(with-eval-after-load 'tempel
  (add-hook 'window-buffer-change-functions #'x/template--file-templates-check-h)
  ;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
  (add-hook 'server-visit-hook #'x/template--file-templates-check-h))

(provide 'x-template)
;;; x-template.el ends here
