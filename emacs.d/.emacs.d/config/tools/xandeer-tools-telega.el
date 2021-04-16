;;; xandeer-tools-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Editor Telega.

;;; Code:

(straight-register-package
 '(telega :host github
          :repo "zevlg/telega.el"
          :branch "telega-tdlib-150"
          ;; :branch "master"
          :files (:defaults "README.md" "etc" "server" "Makefile" "test.el")))

(straight-use-package 'telega)
(leaf telega
  :commands (telega)
  :bind
  ("M-c" . telega-chat-with)
  :bind
  (:telega-chat-mode-map
   ("C-c C-f" . xandeer/telega-chatbuf-attach-file))
  :config
  (telega-mode-line-mode 1)
  (setq telega-chat-fill-column 65)
  (setq telega-emoji-use-images nil)
  (setq
   telega-proxies
   (list
    '(:server "127.0.0.1" :port 8010 :enable t
              :type (:@type "proxyTypeHttp")))
   telega-sticker-set-download t
   telega-chat-button-width 28
   telega-cache-dir (expand-file-name "~/Downloads/telega")
   telega-root-fill-column 48)
  (setq telega-completing-read-function 'ivy-completing-read)

  (defun xandeer/telega-chatbuf-attach-file (filename &optional preview-p)
    "Attach FILE as document to the current input."
    (interactive (list (read-file-name "Attach file: " (expand-file-name "~/temp"))))
    (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
      (telega-chatbuf-input-insert
       (list :@type "inputMessageDocument"
             :document ifile))))

  (defun telega-server--check-version (required-version)
    "Stop check at starting server."
    (let ((ts-version (telega-server-version)))
      (message (format "Installed version: %s, latest version: %s."
                       required-version ts-version))))

(add-hook 'telega-chat-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '(telega-company-emoji
                           telega-company-username
                           telega-company-hashtag)
                         (when (telega-chat-bot-p telega-chatbuf--chat)
                           '(telega-company-botcmd))))))

(when *is-a-mac*
  ;; emacs-mac have some bug on user avatars
  (setq telega-user-use-avatars nil)))

(provide 'xandeer-tools-telega)
;;; xandeer-tools-telega.el ends here
