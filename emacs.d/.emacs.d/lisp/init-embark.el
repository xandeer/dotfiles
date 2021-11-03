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

  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-roam-map))

  (eval-when-compile
    (defmacro xr/embark-split-action (fn split-type)
      `(defun ,(intern (concat "xr/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         (interactive)
         (select-window (funcall #',split-type))
         (call-interactively #',fn))))

  (define-key embark-file-map     (kbd "2") (xr/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "2") (xr/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (xr/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map     (kbd "3") (xr/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "3") (xr/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (xr/embark-split-action bookmark-jump split-window-right)))

(provide 'init-embark)
;;; init-embark.el ends here
