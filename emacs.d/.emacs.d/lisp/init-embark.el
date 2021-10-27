;;; init-embark.el --- init-embark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf marginalia
  :straight t
  :require t
  :hook after-init-hook)

(leaf embark
  :straight t
  :require t
  :bind
  ("C-;" . embark-act)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'init-embark)
;;; init-embark.el ends here
