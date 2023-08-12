;;; x-exec-path.el --- x-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((original-path (getenv "PATH")))
  (unless (string-match-p "sdk" original-path)
    (mapc (lambda (dir)
            (setenv "PATH" (concat (expand-file-name dir) ":" (getenv "PATH")))
            (add-to-list 'exec-path (expand-file-name dir)))
          '("~/.yarn/bin"
            "~/Library/Android/sdk/platform-tools"
            "~/.nix-profile/bin"
            "/etc/profiles/per-user/kevin/bin"
            "/run/current-system/sw/bin"
            "/nix/var/nix/profiles/default/bin"
            "/opt/homebrew/bin"
            "/usr/local/bin"))))

;; (setq exec-path
;;       (mapcar
;;        (lambda
;;          (f)
;;          (if f
;;              (directory-file-name f)
;;            "."))
;;        (append
;;         (parse-colon-path
;;          (getenv "PATH"))
;;         (list exec-directory))))

;; (with-eval-after-load 'dash
;;   (setq exec-path (-distinct exec-path)))

(defun x/prepend-homebrew-path (cmd)
  (concat "PATH=/opt/homebrew/bin:$PATH; " cmd))

(defvar x/process-prefix "x/exec-"
  "Prefix of the subprocess.")

(defun x/process-buffer-get (program)
  "Return the process buffer associated with the PROGRAM."
  (format "*%s%s*" x/process-prefix program))

(defun x/start-process (cmd &optional switch?)
  "Execute CMD in a subprocess.
If SWITCH? is non-nil, switch to the process buffer."
  (let* ((program (car (split-string cmd)))
         (name (concat x/process-prefix program))
         (buffer (x/process-buffer-get program)))
    (apply #'make-process
           `(:name ,name
                   :buffer ,buffer
                   :command ,(mapcar (lambda (it)
                                       (if (s-starts-with? "~" it)
                                           (expand-file-name it)
                                         it))
                                     (split-string cmd))))
    (when switch?
      (switch-to-buffer buffer)
      (goto-char (point-max)))))

(defun x/append-exec-path (args)
  "Append `/opt/homebrew/bin` to PATH with `shell-command`."
  (list (x/prepend-homebrew-path (car args)) (cadr args)))

(advice-add 'shell-command :filter-args #'x/append-exec-path)
(advice-add 'org-babel-eval :filter-args #'x/append-exec-path)
;; (advice-add 'async-shell-command :filter-args #'x/append-exec-path)

(provide 'x-exec-path)
;;; x-exec-path.el ends here
