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
  :after ivy
  :bind
  (:ivy-minibuffer-map
   ("C-o" . embark-act))
  :defer-config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (embark-define-keymap embark-roam-map
    "Keymap with a roam node actions."
    ("o" (xr/embark-ace-action org-roam-node-find))
    ("2" (xr/embark-split-action org-roam-node-find split-window-below))
    ("3" (xr/embark-split-action org-roam-node-find split-window-right)))

  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-roam-map)))

(provide 'init-embark)
;;; init-embark.el ends here
