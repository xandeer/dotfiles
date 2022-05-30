;;; x-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(telega :host github
          :repo "zevlg/telega.el"
          :branch "release-0.8.0"))

;; dependencies: brew install ffmpeg
(require-package 'telega)

(setq telega-root-fill-column 48)
(setq telega-chat-fill-column 65)
(setq telega-chat-scroll-conservatively 101)
(setq telega-sticker-set-download t)
(setq telega-cache-dir (expand-file-name "~/syncthing/personal/telega"))
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

(defun x/telega-chatbuf-attach-file (filename &optional preview-p)
    "Attach FILE as document to the current input."
    (interactive (list (read-file-name "Attach file: " (expand-file-name "~/syncthing/") nil nil nil (lambda (n) (not (string-equal n ".DS_Store"))))))
    (let ((ifile (telega-chatbuf--gen-input-file filename 'Document preview-p)))
      (telega-chatbuf-input-insert
       (list :@type "inputMessageDocument"
             :document ifile))))

(autoload #'x/telega-chat-with "telega" nil t)
(autoload #'x/open-telega-root "telega" nil t)
(with-eval-after-load 'telega
  (defun x/telega-chat-with ()
    "If telega server is live start chatting, else start telega."
    (interactive)
    (if (telega-server-live-p)
        (call-interactively 'telega-chat-with)
      (progn
        (telega)
        (message "Telega starting..."))))

  (defun x/open-telega-root ()
    "If telega server is live open telega root buffer, else start telega."
    (interactive)
    (if (telega-server-live-p)
        (switch-to-buffer telega-root-buffer-name)
      (telega)))

  (define-key telega-chat-mode-map (kbd "C-c C-f") 'x/telega-chatbuf-attach-file)
  (define-key telega-chat-mode-map [remap self-insert-command] #'x-point-self-insert-command)

  (defun x/telega--chat-mode-setup ()
    (setq-local line-spacing 0))

  (add-hook 'telega-chat-mode-hook #'x/telega--chat-mode-setup))

(provide 'x-telega)
;;; x-telega.el ends here
