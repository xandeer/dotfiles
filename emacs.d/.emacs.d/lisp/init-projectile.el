;;; init-projectile.el --- init-projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-use-package 'projectile)
(leaf projectile
  :doc "Projectile is a project interaction library for Emacs."
  :url "https://github.com/bbatsov/projectile"
  :tag "project" "convenience"
  :bind ([remap find-tag] . projectile-find-tag)
  :hook after-init-hook
  :custom
  (projectile-indexing-method      . 'hybrid)
  (projectile-require-project-root . 'prompt)
  :config
  (gsetq projectile-project-root-files-top-down-recurring
         (append '("compile_commands.json"
                   ".cquery")
                 projectile-project-root-files-top-down-recurring)))

(provide 'init-projectile)
;;; init-projectile.el ends here
