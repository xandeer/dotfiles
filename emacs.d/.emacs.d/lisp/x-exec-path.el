;;; x-exec-path.el --- x-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "PATH" (concat
                (expand-file-name "~/bin:")
                (getenv "PATH")
                ":/opt/homebrew/bin:/usr/local/bin:/run/current-system/sw/bin:"
                (expand-file-name "~/.nix-profile/bin/:")
                (expand-file-name "~/Library/Android/sdk/platform-tools/:")))

(setq exec-path
      (mapcar
       (lambda
         (f)
         (if f
             (directory-file-name f)
           "."))
       (append
        (parse-colon-path
         (getenv "PATH"))
        (list exec-directory))))

(with-eval-after-load 'dash
  (setq exec-path (-distinct exec-path)))

(defun x/prepend-homebrew-path (cmd)
  (concat "PATH=$HOME/bin:/opt/homebrew/bin:$PATH; " cmd))

(defvar x/process-prefix "x/exec-"
  "Prefix of the subprocess.")

(defun x/process-buffer-get (program)
  "Return the process buffer associated with the PROGRAM."
  (format "*%s%s*" x/process-prefix program))

(defun x/start-process (cmd)
  "Execute CMD in a subprocess."
  (let* ((program (car (split-string cmd)))
         (name (concat x/process-prefix program))
         (buffer (x/process-buffer-get program)))
    (apply #'make-process
           `(:name ,name
                   :buffer ,buffer
                   :command ,(split-string cmd)))))

(defun x/append-exec-path (args)
  "Append `/opt/homebrew/bin` to PATH with `shell-command`."
  (list (x/prepend-homebrew-path (car args)) (cadr args)))

(advice-add 'shell-command :filter-args #'x/append-exec-path)
;; (advice-add 'async-shell-command :filter-args #'x/append-exec-path)

(provide 'x-exec-path)
;;; x-exec-path.el ends here
