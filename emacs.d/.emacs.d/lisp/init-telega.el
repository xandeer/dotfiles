;;; init-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(telega :host github
          :repo "zevlg/telega.el"
          ;; :commit "9008565"
          :branch "master"))

;; dependencies: brew install ffmpeg
(leaf telega
  :straight t
  :commands (telega)
  :bind
  ("M-c" . xr/telega-chat-with)
  :bind
  (:telega-chat-mode-map
   ("C-c C-f" . xr/telega-chatbuf-attach-file))
  :custom
  (telega-root-fill-column           . 48)
  (telega-chat-fill-column           . 65)
  (telega-chat-scroll-conservatively . 101)
  (telega-sticker-set-download       . t)
  (telega-cache-dir                  . `,(expand-file-name "~/Downloads/telega"))
  (telega-completing-read-function   . 'ivy-completing-read)
  (telega-proxies
   . `,(list
        '(:server "127.0.0.1" :port 8010 :enable t
                  :type (:@type "proxyTypeHttp"))))
  :config
  (defun xr/telega-chat-with ()
    "If telega server is live start chatting, else start telega."
    (interactive)
    (if (telega-server-live-p)
        (call-interactively 'telega-chat-with)
      (progn
        (telega)
        (message "Telega starting..."))))

  (defun xr/telega-chatbuf-attach-file (filename &optional preview-p)
    "Attach FILE as document to the current input."
    (interactive (list (read-file-name "Attach file: " (expand-file-name "~/temp"))))
    (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
      (telega-chatbuf-input-insert
       (list :@type "inputMessageDocument"
             :document ifile))))
  ;; Deprecated
  (defun xr/telega-server--check-version (required-version)
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
                             '(telega-company-botcmd)))))))

(provide 'init-telega)
;;; init-telega.el ends here
