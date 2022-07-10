;;; x-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(unless doom-version
  (straight-register-package
   '(telega :host github
            :repo "zevlg/telega.el"
            :branch "release-0.8.0")))

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
  (if (telega-server-live-p)
      (let ((file-mime (or (mailcap-extension-to-mime
                            (or (file-name-extension filename) ""))
                           "telega/unknown")))
        (cond ((string= "image/gif" file-mime)
               (telega-chatbuf-attach-animation filename))
              ((string-prefix-p "image/" file-mime)
               (telega-chatbuf-attach-photo filename))
              ((string-prefix-p "audio/" file-mime)
               (telega-chatbuf-attach-audio filename))
              ((string-prefix-p "video/" file-mime)
               (telega-chatbuf-attach-video filename))
              ((string-match-p "^https?://" filename)
               (telega-chatbuf-input-insert filename))
              (t
               (telega-chatbuf-attach-file filename t))))))

(defun x/telega-send-to-chat (&optional filename)
  "Attach FILE as document to the chat input."
  (interactive)
  (setq filename (or filename
                     (and (equal major-mode 'dired-mode) (dired-get-filename))
                     (buffer-file-name)))
  (call-interactively 'x/telega-chat-with)
  (if (telega-server-live-p)
      (let ((file-mime (or (mailcap-extension-to-mime
                            (or (file-name-extension filename) ""))
                           "telega/unknown")))
        (cond ((string= "image/gif" file-mime)
               (telega-chatbuf-attach-animation filename))
              ((string-prefix-p "image/" file-mime)
               (telega-chatbuf-attach-photo filename))
              ((string-prefix-p "audio/" file-mime)
               (telega-chatbuf-attach-audio filename))
              ((string-prefix-p "video/" file-mime)
               (telega-chatbuf-attach-video filename))
              ((string-match-p "^https?://" filename)
               (telega-chatbuf-input-insert filename))
              (t
               (telega-chatbuf-attach-file filename t))))))

(autoload #'x/telega-send-to-chat "telega" nil t)
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
  ;; (define-key telega-chat-mode-map [remap self-insert-command] #'x-point-self-insert-command)

  (defun x/telega--chat-mode-setup ()
    (setq-local line-spacing 0))

  (add-hook 'telega-chat-mode-hook #'x/telega--chat-mode-setup))

(provide 'x-telega)
;;; x-telega.el ends here
