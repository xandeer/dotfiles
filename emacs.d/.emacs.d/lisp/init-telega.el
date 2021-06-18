;;; init-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(telega :host github
          :repo "zevlg/telega.el"
          ;; :commit "9008565"
          :branch "releases"))

;; dependencies: brew install tdlib ffmpeg
(leaf telega
  :straight t
  :commands (telega)
  :bind
  ("M-c" . xr/telega-chat-with)
  :bind
  (:telega-chat-mode-map
   ("C-c C-f" . xr/telega-chatbuf-attach-file))
  :config
  (telega-mode-line-mode 1)
  (setq telega-chat-fill-column 65)
  (setq telega-emoji-use-images nil)
  (setq-default telega-chat-scroll-scroll-conservatively 101)
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

  (defun xr/telega-chat-with ()
    "If telega server is live start chatting, else start telega."
    (interactive)
    (if (telega-server-live-p)
        (progn
          (xr/enable-pinyin)
          (call-interactively 'telega-chat-with))
      (progn
        (telega 1)
        (message "Telega starting..."))))

  (defun xr/telega-chatbuf-attach-file (filename &optional preview-p)
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

(provide 'init-telega)
;;; init-telega.el ends here
