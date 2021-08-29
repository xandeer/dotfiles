;;; init-exec-path.el --- init-exec-path -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setenv "PATH" (concat
                (getenv "PATH")
                ":/usr/local/bin:/run/current-system/sw/bin:"
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

(provide 'init-exec-path)
;;; init-exec-path.el ends here
