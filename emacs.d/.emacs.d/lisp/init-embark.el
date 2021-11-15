;;; init-embark.el --- init-embark -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'marginalia)
(add-hook 'after-init-hook 'marginalia-mode)

(require-package 'embark)

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(eval-when-compile
  (defmacro xr/embark-ace-action (fn)
    `(defun ,(intern (concat "xr/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (aw-switch-to-window (aw-select nil))
         (call-interactively (symbol-function ',fn))))))

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

(with-eval-after-load 'marginalia
  (with-eval-after-load 'embark
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))

    (embark-define-keymap embark-roam-map
      "Keymap with a roam node actions."
      ("2" (xr/embark-split-action org-roam-node-find split-window-below))
      ("3" (xr/embark-split-action org-roam-node-find split-window-right)))

    (add-to-list 'embark-keymap-alist '(org-roam-node . embark-roam-map))

    ;; (add-to-list 'marginalia-prompt-categories '("Find file: " . file))

    (with-eval-after-load 'consult
      (require-package 'embark-consult)
      (require 'embark-consult))

    (with-eval-after-load 'ace-window
      (define-key embark-file-map     (kbd "o") (xr/embark-ace-action find-file))
      (define-key embark-roam-map     (kbd "o") (xr/embark-ace-action org-roam-node-find))
      (define-key embark-buffer-map   (kbd "o") (xr/embark-ace-action switch-to-buffer))
      (define-key embark-bookmark-map (kbd "o") (xr/embark-ace-action bookmark-jump)))

    (define-key embark-file-map     (kbd "2") (xr/embark-split-action find-file split-window-below))
    (define-key embark-buffer-map   (kbd "2") (xr/embark-split-action switch-to-buffer split-window-below))
    (define-key embark-bookmark-map (kbd "2") (xr/embark-split-action bookmark-jump split-window-below))

    (define-key embark-file-map     (kbd "3") (xr/embark-split-action find-file split-window-right))
    (define-key embark-buffer-map   (kbd "3") (xr/embark-split-action switch-to-buffer split-window-right))
    (define-key embark-bookmark-map (kbd "3") (xr/embark-split-action bookmark-jump split-window-right)))

  ;; (global-set-key (kbd "H-i") 'embark-act)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-o") 'embark-act)))

(provide 'init-embark)
;;; init-embark.el ends here
