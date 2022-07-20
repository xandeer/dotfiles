;;; x-sh.el --- x-sh -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; eshell
(with-eval-after-load 'org
  (setq eshell-aliases-file (x/expand-note "etc/eshell.alias"))
  (setq eshell-rc-script (x/expand-note "etc/eshell.profile")))

(setq sh-shell-file "/bin/sh")

(with-eval-after-load 'eshell
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

(with-eval-after-load 'eshell
  (add-hook 'eshell-mode-hook (lambda () (require 'eshell-z))))

;;; async commands
(defvar x/sh-async-command-history nil
  "The history list for async shell commands.")

(defun x/sh-exec-async ()
  "Execute a shell command asynchronously."
  (interactive)
  (let ((command (completing-read "Command: "
                                  x/sh-async-command-history
                                  nil nil nil
                                  'x/sh-async-command-history)))
    (x/start-process command)))

(defvar x/sh-adb-history nil
  "The history list for adb connect functions.")

(defun x/sh-adb-connect ()
  "Connect to an android device."
  (interactive)
  (let ((ip (completing-read "Adb Connect: "
                             x/sh-adb-history
                             nil nil
                             (concat (s-join "." (butlast (s-split "\\." (x/ifconfig)))) ".")
                             'x/sh-adb-history)))
    (x/start-process
     (concat "adb connect " ip))))

(autoload 'osx-lib-get-from-clipboard "osx-lib")
(defun x/sh-run-clipboard ()
  "Call `x/start-process' with the clipboard content."
  (interactive)
  (x/start-process (osx-lib-get-from-clipboard)))

(provide 'x-sh)
;;; x-sh.el ends here
