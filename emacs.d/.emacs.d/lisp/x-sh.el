;;; x-sh.el --- x-sh -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'company-shell)
(with-eval-after-load 'company
  (with-eval-after-load 'sh-script
    (add-to-list 'company-backends 'company-shell)))

(with-eval-after-load 'org
  (setq eshell-aliases-file (x/expand-note "etc/eshell.alias")))

(with-eval-after-load 'eshell
  (with-eval-after-load 'company
    (add-hook 'eshell-mode-hook (lambda () (company-mode -1))))

  (with-eval-after-load 'consult
    (defvar consult--eshell-history nil)
    (defun x/consult-eshell-history ()
      "Jump to an eshell history."
      (interactive)
      (require 'em-hist)
      (let* ((completion-beg (eshell-bol))
             (completion-end (point-at-eol))
             (input (buffer-substring-no-properties
                     completion-beg
                     completion-end))
             (cand (delete-dups
                    (when (> (ring-size eshell-history-ring) 0)
                      (ring-elements eshell-history-ring)))))
        (end-of-line)
        (let ((his (consult--read
                    cand
                    :prompt "Eshell history: "
                    :initial input
                    :sort nil
                    :require-match t
                    :history '(:input consult--eshell-history)
                    :add-history (thing-at-point 'symbol)
                    :state (consult--insertion-preview completion-beg completion-end))))
          (when (minibufferp)
            (delete-minibuffer-contents))
          (eshell-kill-input)
          (insert (substring-no-properties his)))))
    (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map [(control ?s)] #'x/consult-eshell-history)))))

(require-package 'eshell-z)
(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda () (require 'eshell-z))))

(require-package 'makefile-executor)

(provide 'x-sh)
;;; x-sh.el ends here
