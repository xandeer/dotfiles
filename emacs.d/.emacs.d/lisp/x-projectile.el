;;; x-projectile.el --- x-projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; config
(setq projectile-indexing-method      'hybrid)
(setq projectile-require-project-root 'prompt)
(with-eval-after-load 'projectile
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".cquery")
                projectile-project-root-files-top-down-recurring))

  (defun x/projectile-find-file-external ()
    "Open a project file with the external program."
    (interactive)
    (let* ((project-root (projectile-acquire-root))
           (file (projectile-completing-read "Find file external: "
                                             (projectile-project-files project-root))))
      (when file
        (x/open (expand-file-name file project-root))))))

(defun x/find-file-external (filename &optional wildcards)
  "Open a file with the external program."
  (interactive
   (find-file-read-args "Find file external: "
                        (confirm-nonexistent-file-or-buffer)))
  (x/open (expand-file-name filename)))

(x/append-init-hook #'projectile-mode)

;;; keybindings
(define-prefix-command 'x/projectile-map)
(x/define-keys global-map
            '(("C-c p" x/projectile-map)
              ("C-x C-b" projectile-switch-to-buffer)
              ("H-p" projectile-find-file)))

(with-eval-after-load 'project
  (global-set-key (kbd "C-c p") #'x/projectile-map))

(x/define-keys
 x/projectile-map
 '(("b" projectile-switch-to-buffer)
   ("d" projectile-find-dir)
   ("e" x/projectile-find-file-external)
   ("f" projectile-find-file)
   ("p" projectile-switch-project)
   ("r" projectile-recentf)
   ("s" projectile-run-eshell)))

(with-eval-after-load 'consult
  (defun x/search-in-project ()
    (interactive)
    (consult-ripgrep (projectile-project-root)))
  (define-key x/projectile-map "s" #'x/search-in-project))

(provide 'x-projectile)
;;; x-projectile.el ends here
