;;; x-utils.el --- x-utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

;; (global-set-key (kbd "H-e") 'x/kill-other-window-buffer)
(global-set-key (kbd "H-b") (lambda () (interactive) (switch-to-buffer "*scratch*")))

(defun x/expand-repo (path)
  "Expand PATH in ~/prejects/personal ."
  (expand-file-name path "~/projects/personal"))

(defun x/expand-note (path)
  "Expand PATH in `org-directory`."
  (expand-file-name path (x/expand-repo "notes")))

(defun x/ifconfig ()
  (interactive)
  (message (format-network-address
            (or
             (car (network-interface-info "en0"))
             (car (network-interface-info "en1")))
            t)))

(defun x/string-append-time-suffix (string)
  "Append a time suffix to STRING."
  (concat string (format-time-string "%F-%H-%M-%S%3N")))

(defun x/launch-separate-emacs-under-x ()
  "Launch a separate Emacs instance under X."
  (interactive)
  (call-process "sh" nil nil nil "-c" "emacs &"))

;;; exercism
(defun x/exercism-submit ()
  "Submit the current buffer to Exercism."
  (interactive)
  (shell-command (format "exercism submit %s"
                         (buffer-file-name))))

(defun x/exercism-open-readme-other-window ()
  "Open the README related with current file."
  (interactive)
  (let ((root (locate-dominating-file (buffer-file-name) "README.md")))
    (find-file-other-window (expand-file-name "README.md" root))))

;;; dots
(defun x/makefile-executor-execute (filename)
  "Execute a Makefile target from FILENAME.

FILENAME defaults to current buffer."
  (let ((target (makefile-executor-select-target filename)))
    (makefile-executor-store-cache filename target)
    (compile (format "make -f %s -C %s %s"
                     (shell-quote-argument filename)
                     (shell-quote-argument (file-name-directory filename))
                     target))))

(autoload 'makefile-executor-select-target "makefile-executor")

(defun x/dots ()
  "Run make with dotfiles/Makefile."
  (interactive)
  (x/makefile-executor-execute (x/expand-repo "dotfiles/Makefile")))

;; copy from https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
;; Like diminish, but for major modes
(defun x/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(autoload 'derived-mode-hook-name "derived")
(defun x/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'x/set-major-mode-name name)))

;;; emacs
(defun x/restart-emacs ()
  "Restart Emacs."
  (interactive)
  (let* ((launch-emacs-under-x (lambda () (call-process "sh" nil nil nil "-c" "emacs &")))
         (kill-emacs-hook (append kill-emacs-hook `(,launch-emacs-under-x))))
    (save-buffers-kill-emacs)))

;;; keybindings
(defun x/define-keys (map bindings)
  "Define keys in MAP according to BINDINGS.

Example:
  (x/define-keys org-agenda-mode-map
                 (quote ((\"k\" org-agenda-previous-item)
                         (\"p\" org-agenda-previous-item)
                         (\"n\" org-agenda-next-item)
                         (\"j\" org-agenda-next-item)
                         (\"T\" org-agenda-goto-today)
                         (\"i\" org-agenda-clock-in)
                         (\"o\" org-agenda-clock-goto)
                         ([remap org-schedule] x/org-schedule)))"
  (mapc (lambda (binding)
          (let* ((key (car binding))
                 (key (if (stringp key)
                          (kbd key)
                        key))
                 (command (cadr binding)))
            (define-key map key command)))
        bindings))

(provide 'x-utils)
;;; x-utils.el ends here
