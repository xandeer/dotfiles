;;; init-projectile.el --- init-projectile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf projectile
  :straight t
  :bind
  ([remap find-tag] . projectile-find-tag)
  ("H-p"     . counsel-projectile-find-file)
  ("C-c p f" . counsel-projectile-find-file)
  ("C-c p p" . projectile-switch-project)
  ("C-c p s" . xr/ivy/project-search)
  :hook after-init-hook
  :custom
  (projectile-indexing-method      . 'hybrid)
  (projectile-require-project-root . 'prompt)
  :config
  (setq projectile-project-root-files-top-down-recurring
         (append '("compile_commands.json"
                   ".cquery")
                 projectile-project-root-files-top-down-recurring)))

(provide 'init-projectile)
;;; init-projectile.el ends here
