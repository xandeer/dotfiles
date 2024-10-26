;;; x-console.el --- console -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Eat configuration
(x/package-use
 '(emacs-eat . ("https://codeberg.org/akib/emacs-eat.git" :host nil
                :files ("*.el" ("term" "term/*.el") "*.texi"
                        "*.ti" ("terminfo/e" "terminfo/e/*")
                        ("terminfo/65" "terminfo/65/*")
                        ("integration" "integration/*")
                        (:exclude ".dir-locals.el" "*-tests.el")))))

(with-eval-after-load 'emacs-eat
  (setq eat-shell "nu"
        eat-kill-buffer-on-exit t)
  (add-hook 'eat-mode-hook (x/meow-insert-with-timer 0.1)))

;;; Eshell configuration
(setq sh-shell-file "/bin/sh"
      eshell-scroll-to-bottom-on-input 'this)

(with-eval-after-load 'org
  (setq eshell-aliases-file (x/expand-note "etc/eshell.alias")
        eshell-rc-script (x/expand-note "etc/eshell.profile")))

(with-eval-after-load 'eshell
  (eat-eshell-mode)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (require 'eshell-z)
              (define-key eshell-mode-map [(control ?s)] #'x/consult-eshell-history))))

(with-eval-after-load 'consult
  (defvar consult--eshell-history nil)
  (defun x/consult-eshell-history ()
    "Jump to an eshell history."
    (interactive)
    (require 'em-hist)
    (goto-char (point-max))
    (let* ((completion-beg (point-at-bol))
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
        (insert (substring-no-properties his))))))

(provide 'x-console)
;;; x-console.el ends here
