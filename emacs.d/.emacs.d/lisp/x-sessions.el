;;; x-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'no-littering)
(require 'x-init-utils)

;; save a list of open files in ~/.emacs.d/var/.emacs.desktop
(setq desktop-path (list no-littering-var-directory)
      desktop-auto-save-timeout 600)
;; (desktop-save-mode 1)

(defun x/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun x/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (x/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'x/desktop-time-restore)

(defun x/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (x/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'x/desktop-time-buffer-create)

(require 'savehist)
(x/append-init-hook #'savehist-mode)
(setq history-length 100)

(add-to-list 'savehist-additional-variables '(kill-ring . 20))
(add-to-list 'savehist-additional-variables '(regexp-search-ring . 20))

(x/append-init-hook #'session-initialize)
(setq session-save-file (no-littering-expand-var-file-name "session"))
(setq session-name-disable-regexp "\\(?:\\`'/tmp\\|notes/journal/.+\\|\\.git/[A-Z_]+\\'\\)")
(setq session-save-file-coding-system 'utf-8)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 10)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))

(provide 'x-sessions)

;;; x-sessions.el ends here
