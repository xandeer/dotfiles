;;; x-exec-path.el --- x-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "PATH" (concat
                (getenv "PATH")
                ":/opt/homebrew/bin:/usr/local/bin:/run/current-system/sw/bin:"
                ;; (expand-file-name "~/.nix-profile/bin/:")
                (expand-file-name "~/Library/Android/sdk/platform-tools/:")
                (expand-file-name "~/bin")))

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

(defun x/async-command (cmd)
  (interactive)
  (async-shell-command cmd "*x/async*"))

(defun x/append-exec-path (args)
  "Append `/opt/homebrew/bin` to PATH with `shell-command`."
  (list (concat "PATH=/opt/homebrew/bin:$PATH; " (car args)) (cadr args)))

(advice-add 'shell-command :filter-args #'x/append-exec-path)

(provide 'x-exec-path)
;;; x-exec-path.el ends here
