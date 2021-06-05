;;; init-pass.el --- init-pass -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(pass
   :host github
   :repo "NicolasPetton/pass"
   :tag "2.0"))
(straight-register-package
 '(helm-pass
   :host github
   :repo "emacs-helm/helm-pass"))

(leaf pass
  :straight pass helm-pass
  :config
  (setf epg-pinentry-mode 'loopback))

(provide 'init-pass)
;;; init-pass.el ends here
