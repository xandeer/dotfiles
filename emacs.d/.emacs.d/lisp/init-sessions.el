;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; save a list of open files in ~/.emacs.d/var/.emacs.desktop
(setq desktop-path (list no-littering-var-directory)
      desktop-auto-save-timeout 600)
;(desktop-save-mode 1)

(defun xr/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun xr/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (xr/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'xr/desktop-time-restore)

(defun xr/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (xr/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'xr/desktop-time-buffer-create)

;; Restore histories and registers after saving
(setq-default history-length 10)
(require 'savehist)
(add-hook 'after-init-hook 'savehist-mode)

(leaf session
  :straight t
  :hook (after-init-hook . session-initialize)
  :config
  (setq session-save-file (no-littering-expand-var-file-name "session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|notes/journal/.+\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8))

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
        (ivy-history              . 100)
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

(provide 'init-sessions)

;;; init-sessions.el ends here
