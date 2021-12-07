;;; x-init-utils.el --- x-init-utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/append-init-hook (fn)
  "Add FN as a hook on `after-init-hook`."
  (add-hook 'after-init-hook fn))

(defvar x--auto-timer nil)
(defvar x--auto-push-answered? t)
(defun x/auto-session ()
  (run-with-idle-timer
   5 nil (lambda ()
           (run-with-idle-timer
            1 nil (lambda ()
                    (eva-query-mood)))
           (if x--auto-push-answered?
               (progn
                 (setq x--auto-push-answered? nil)
                 (run-with-idle-timer
                  3 nil (lambda ()
                          (when (y-or-n-p "Push notes to github? ")
                            (async-shell-command
                             (concat "cd " org-directory
                                     "; git add --all && git commit -m 'emacs timer: "
                                     (format-time-string "[%F %a %T]'")
                                     "; git push")))
                          (setq x--auto-push-answered? t)))))
           (setq x--auto-timer
                 (run-with-timer 3600 nil #'x/auto-session)))))

(defun x/disable-auto-session ()
  "Disalbe auto session."
  (interactive)
  (when (timerp x--auto-timer)
    (setq x--auto-timer (cancel-timer x--auto-timer))))

(provide 'x-init-utils)
;;; x-init-utils.el ends here
