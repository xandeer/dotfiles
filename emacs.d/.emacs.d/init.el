;;; init.el --- Xandeer's Emacs Configuration file. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq-default lexical-binding t)

;;------------------------------------------------------------------------------
;; Bootstrap
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))
(require 'xandeer-bootstrap)

(xandeer/local-repo core)
(require 'xandeer-core)

(xandeer/local-repo editor)
(require 'xandeer-editor)

(xandeer/local-repo tools)
(require 'xandeer-tools)

(xandeer/local-repo langs)
(require 'xandeer-langs)

(xandeer/local-repo org)
(require 'xandeer-org)

(xandeer/local-repo ui)
(require 'xandeer-ui)

;; Bootstrap config
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-frame-hooks)
(require 'init-gui-frames)
(require 'init-recentf)
(require 'init-sessions)

(require 'init-meow)
;;----------------------------------------------------------------------------
;; Epilogue
;;----------------------------------------------------------------------------

(run-hooks 'xandeer/config-before-hook)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(add-hook 'after-init-hook #'(lambda () (run-hooks 'xandeer/config-after-hook)))

(when (file-exists-p custom-file)
  (load custom-file))

(when *server*
  (server-start))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
