;;; x-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(telega :host github
          :repo "zevlg/telega.el"
          ;; :commit "9008565"
          :branch "master"))

;; dependencies: brew install ffmpeg
(require-package 'telega)

(setq telega-root-fill-column 48)
(setq telega-chat-fill-column 65)
(setq telega-chat-scroll-conservatively 101)
(setq telega-sticker-set-download t)
(setq telega-cache-dir (expand-file-name "~/Downloads/telega"))
(setq telega-completing-read-function 'completing-read)
(setq telega-proxies
      (list '(:server "127.0.0.1" :port 8010 :enable t
                      :type (:@type "proxyTypeHttp"))))
;; Deprecated
(defun x/telega-server--check-version (required-version)
  "Stop check at starting server."
  (let ((ts-version (telega-server-version)))
    (message (format "Installed version: %s, latest version: %s."
                     required-version ts-version))))

(autoload #'x/telega-chat-with "telega" nil t)

(defun x/telega-chatbuf-attach-file (filename &optional preview-p)
    "Attach FILE as document to the current input."
    (interactive (list (read-file-name "Attach file: " (expand-file-name "~/temp/"))))
    (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
      (telega-chatbuf-input-insert
       (list :@type "inputMessageDocument"
             :document ifile))))

(with-eval-after-load 'telega
  (defun x/telega-chat-with ()
    "If telega server is live start chatting, else start telega."
    (interactive)
    (if (telega-server-live-p)
        (call-interactively 'telega-chat-with)
      (progn
        (telega)
        (message "Telega starting..."))))

  (define-key telega-chat-mode-map (kbd "C-c C-f") 'x/telega-chatbuf-attach-file)

  (add-hook 'telega-chat-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '(telega-company-emoji
                             telega-company-username
                             telega-company-hashtag)
                           (when (telega-chat-bot-p telega-chatbuf--chat)
                             '(telega-company-botcmd)))))))

(provide 'x-telega)
;;; x-telega.el ends here