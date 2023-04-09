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

(add-hook 'typescript-mode-hook #'tempel-abbrev-mode)
;; `expand-abbrev` is bound to "C-x '".
;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;; (global-tempel-abbrev-mode)

;;; key bindings
(define-key tempel-map (kbd "TAB") #'tempel-next)
(define-key tempel-map (kbd "C-<tab>") #'tempel-previous)
(define-key tempel-map (kbd "C-g") #'tempel-done)
(define-key tempel-map (kbd "C-c") #'tempel-abort)
(define-key tempel-map (kbd "M-a") #'tempel-beginning)
(define-key tempel-map (kbd "M-e") #'tempel-end)
(global-set-key (kbd "M-C") #'tempel-complete)

;;; org
(define-transient-command x/transient-template-org ()
  "Transient for Org Templates."
  [["Src"
    ("M-s" "Src" ,(x/interactive-wrapper (tempel-insert 'src)))
    ("e" "Elisp" ,(x/interactive-wrapper (tempel-insert 'elisp)))
    ("s" "Sh" ,(x/interactive-wrapper (tempel-insert 'sh)))
    ("k" "Kotlin" ,(x/interactive-wrapper (tempel-insert 'kotlin)))
    ("c" "Clojure" ,(x/interactive-wrapper (tempel-insert 'clojure)))]
   ["Review"
    ("r" "Weekly review" ,(x/interactive-wrapper (tempel-insert 'weekly)))
    ("m" "Monthly review" ,(x/interactive-wrapper (tempel-insert 'monthly)))]
   ["Block"
    (";" "Comment" ,(x/interactive-wrapper (tempel-insert 'comments)))
    ("q" "Quote" ,(x/interactive-wrapper (tempel-insert 'quote)))
    ("v" "Verse" ,(x/interactive-wrapper (tempel-insert 'verse)))
    ("w" "Wrap block" x/wrap-block)]
   ["Other"
    ("M-t" "Tempel insert" tempel-insert)
    ("t" "Timestamp" ,(x/interactive-wrapper (tempel-insert 'time)))
    ("d" "Day: 14(Fri)" ,(x/interactive-wrapper (tempel-insert 'day)))]])

(define-key org-mode-map (kbd "M-t") #'x/transient-template-org)

;;; elisp
(define-transient-command x/transient-template-elisp ()
  "Transient for Elisp Templates."
  [["Elisp Templates"
    ("d" "Elisp header and footer" ,(x/interactive-wrapper (tempel-insert 'file-template)))
    ("l" "Lambda" ,(x/interactive-wrapper (tempel-insert 'lambda)))
    ("f" "Defun" ,(x/interactive-wrapper (tempel-insert 'fun)))
    ("c" "Command" ,(x/interactive-wrapper (tempel-insert 'command)))
    ("M-t" "Tempel insert" tempel-insert)
    ("M-l" "Let" ,(x/interactive-wrapper (tempel-insert 'let)))
    ("v" "Defvar" ,(x/interactive-wrapper (tempel-insert 'var)))
    ("M-c" "Defconst" ,(x/interactive-wrapper (tempel-insert 'const)))
    ("S-c" "Defcustom" ,(x/interactive-wrapper (tempel-insert 'custom)))]])

(define-key emacs-lisp-mode-map (kbd "M-t") #'x/transient-template-elisp)

;;; global
(define-transient-command x/transient-template-global ()
  "Transient for Global Templates."
  [["Global Templates"
    ("d" "Git changelog" ,(x/interactive-wrapper (tempel-insert 'chglog)))
    ("v" "Git app version" ,(x/interactive-wrapper (tempel-insert 'version)))
    ("t" "Timestamp" ,(x/interactive-wrapper (tempel-insert 'time)))
    ("M-t" "Tempel insert" tempel-insert)]])

(global-set-key (kbd "M-t") #'x/transient-template-global)

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
