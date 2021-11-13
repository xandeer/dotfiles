;;; init-projectile.el --- init-projectile -*- lexical-binding: t -*-
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
  (global-set-key (kbd "C-c p p") 'projectile-switch-project)

  (with-eval-after-load 'consult
    (defun xr/search-in-project ()
      (interactive)
      (consult-ripgrep (projectile-project-root)))
    (global-set-key (kbd "C-c p s") 'xr/search-in-project))

  (add-hook 'after-init-hook 'projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
