;;; x-projectile.el --- x-projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'projectile)
(require 'projectile)

(with-eval-after-load 'projectile
  (setq projectile-indexing-method      'hybrid)
  (setq projectile-require-project-root 'prompt)
  (setq projectile-project-root-files-top-down-recurring
        (append '("compile_commands.json"
                  ".cquery")
                projectile-project-root-files-top-down-recurring))

  (global-set-key (kbd "H-p") 'projectile-find-file)
  (global-set-key (kbd "C-c p f") 'projectile-find-file)
  (global-set-key (kbd "C-c f f") 'projectile-find-file)
  (global-set-key (kbd "C-c p p") 'projectile-switch-project)

  (with-eval-after-load 'consult
    (defun x/search-in-project ()
      (interactive)
      (consult-ripgrep (projectile-project-root)))
    (global-set-key (kbd "C-c p s") 'x/search-in-project))

  (x/append-init-hook 'projectile-mode))

(provide 'x-projectile)
;;; x-projectile.el ends here
