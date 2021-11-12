;;; init-embark.el --- init-embark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'marginalia)
(add-hook 'after-init-hook 'marginalia-mode)

(require-package 'embark)

(with-eval-after-load 'marginalia
  (with-eval-after-load 'embark
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
    ;; (add-to-list 'embark-keymap-alist '(counsel . embark-roam-map))
    (add-to-list 'marginalia-prompt-categories '("Recentf" . recentf))

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

    (embark-define-keymap embark-recentf-map
      "Keymap for recentf actions."
      ("o" (xr/embark-ace-action counsel-recentf))
      ("2" (xr/embark-split-action counsel-recentf split-window-below))
      ("3" (xr/embark-split-action counsel-recentf split-window-right)))
    (add-to-list 'embark-keymap-alist '(recentf . embark-recentf-map))

    (define-key embark-file-map     (kbd "2") (xr/embark-split-action find-file split-window-below))
    (define-key embark-buffer-map   (kbd "2") (xr/embark-split-action switch-to-buffer split-window-below))
    (define-key embark-bookmark-map (kbd "2") (xr/embark-split-action bookmark-jump split-window-below))

    (define-key embark-file-map     (kbd "3") (xr/embark-split-action find-file split-window-right))
    (define-key embark-buffer-map   (kbd "3") (xr/embark-split-action switch-to-buffer split-window-right))
    (define-key embark-bookmark-map (kbd "3") (xr/embark-split-action bookmark-jump split-window-right)))

  ;; (global-set-key (kbd "H-i") 'embark-act)
  ;; (define-key ivy-mode-map (kbd "C-o") 'embark-act)
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "C-o") 'embark-act)))

(provide 'init-embark)
;;; init-embark.el ends here
