;;; xandeer-core-mixed.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Configuration Core Mixed.

;;; Code:


(setq enable-recursive-minibuffers t)

(add-hook #'after-init-hook #'(lambda () (minibuffer-depth-indicate-mode 1)))

;; https://www.reddit.com/r/emacs/comments/4d8gvt/how_do_i_automatically_close_the_minibuffer_after/
(defun helper/kill-minibuffer ()
  "Exit the minibuffer if it is active."
  (when (and (>= (recursion-depth) 1)
           (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook #'mouse-leave-buffer-hook #'helper/kill-minibuffer)

(defun xandeer/shell-command-in-view-mode (start end command &optional output-buffer replace &rest other-args)
  "Put \"*Shell Command Output*\" buffers into view-mode."
  (unless (or output-buffer replace)
    (with-current-buffer "*Shell Command Output*"
      (view-mode 1))))
(advice-add 'shell-command-on-region :after 'xandeer/shell-command-in-view-mode)

(straight-use-package 'exec-path-from-shell)
(leaf exec-path-from-shell
  :init
  ;; Non-Forking Shell Command To String
  ;; https://github.com/bbatsov/projectile/issues/1044
  ;;--------------------------------------------------------------------------

  (defun call-process-to-string (program &rest args)
    (with-temp-buffer
      (apply 'call-process program nil (current-buffer) nil args)
      (buffer-string)))

  (defun get-call-process-args-from-shell-command (command)
    (cl-destructuring-bind
        (the-command . args) (split-string command " ")
      (let ((binary-path (executable-find the-command)))
        (when binary-path
          (cons binary-path args)))))

  (defun shell-command-to-string (command)
    (let ((call-process-args
           (get-call-process-args-from-shell-command command)))
      (if call-process-args
          (apply 'call-process-to-string call-process-args)
        (shell-command-to-string command))))

  (defun try-call-process (command)
    (let ((call-process-args
           (get-call-process-args-from-shell-command command)))
      (if call-process-args
          (apply 'call-process-to-string call-process-args))))

  (advice-add 'shell-command-to-string :before-until 'try-call-process)

  (defun call-with-quick-shell-command (fn &rest args)
    (noflet ((shell-command-to-string
              (&rest args)
              (or (apply 'try-call-process args) (apply this-fn args))))
            (apply fn args)))

  (advice-add 'projectile-find-file :around 'call-with-quick-shell-command)
  :custom
  (shell-command-switch . "-ic")
  (shell-file-name      . "zsh")
  ((exec-path-from-shell-arguments
    exec-path-from-shell-check-startup-files) . nil)
  :config
  (exec-path-from-shell-initialize))

(straight-use-package 'default-text-scale)
(leaf default-text-scale
  :commands default-text-scale-mode
  :hook after-init-hook)

(provide 'xandeer-core-mixed)
;;; xandeer-core-mixed.el ends here
