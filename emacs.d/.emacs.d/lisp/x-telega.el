;;; x-telega.el --- Xandeer's Emacs Configuration tools telega file.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Dependencies: brew install ffmpeg

(require 'x-telega-cus-edit-patch)

(setq telega-root-fill-column 48)
(setq telega-chat-fill-column 65)
(setq telega-chat-scroll-conservatively 101)
(setq telega-sticker-set-download t)
(setq telega-cache-dir (expand-file-name "~/syncthing/personal/telega"))
(setq telega-completing-read-function #'completing-read)
(setq telega-chat-input-markups '(nil "org" "markdown2"))
(setq telega-translate-to-language-by-default "zh-CN")
;; (setq telega-proxies
;;       (list '(:server "127.0.0.1" :port 8010 :enable t
;;                       :type (:@type "proxyTypeSocks5"))))

(with-eval-after-load 'telega-customize
  ;; Solve the avatar cut-off problem
  (setf (alist-get 2 telega-avatar-factors-alist) '(0.4 . 0.1)))

(defvar x/telega-chatbuf-attach-dirs
  '("Downloads/"
    "temp/"
    "syncthing/"
    "syncthing/personal/temp/"))

(defun x/telega-chatbuf-attach-select-dir ()
  (expand-file-name (completing-read "Dir: "
                                     x/telega-chatbuf-attach-dirs
                                     nil nil nil
                                     'x/telega-chatbuf-attach-dirs)
                    "~"))

(defun x/telega-chatbuf-attach-file (filename &optional preview-p)
  "Attach FILE as document to the current input."
  (interactive (list
                (read-file-name "Attach file: "
                                (x/telega-chatbuf-attach-select-dir)
                                nil nil nil
                                (lambda (name) (not (string-equal name ".DS_Store"))))))
  (require 'telega)
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

;; (autoload #'x/telega-send-to-chat "telega" nil t)
;; (autoload #'x/telega-chat-with "telega" nil t)
;; (autoload #'x/open-telega-root "telega" nil t)

(defun x/telega-send-to-chat (&optional filename)
  "Attach FILE as document to the chat input."
  (interactive)
  (setq filename (or filename
                     (and (or (equal major-mode 'dired-mode)
                              (equal major-mode 'dirvish-mode)) (dired-get-filename))
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

(defun x/open-telega-root ()
  "If telega server is live open telega root buffer, else start telega."
  (interactive)
  (require 'telega)
  (if (telega-server-live-p)
      (switch-to-buffer telega-root-buffer-name)
    (telega)))

(defun x/telega-chat-with ()
  "If telega server is live start chatting, else start telega."
  (interactive)
  (require 'telega)
  (if (telega-server-live-p)
      (call-interactively 'telega-chat-with)
    (progn
      (telega)
      (message "Telega starting..."))))

(with-eval-after-load 'telega-chat
  (defun x/telega--chat-mode-setup ()
    (setq-local line-spacing 0)
    ;; Treat the Chinese colon as a word separator
    (modify-syntax-entry ?\uff1a "." (syntax-table)))

  (add-hook 'telega-chat-mode-hook #'x/telega--chat-mode-setup)

  (x/define-keys telega-chat-mode-map
                 '(("C-c C-f" x/telega-chatbuf-attach-file)))

  (x/define-keys telega-chat-button-map
                 '(("d" scroll-up-command)
                   ("e" scroll-down-command)
                   ("f" x/telega-chat-with))))

(with-eval-after-load 'telega-msg
  ;; (set-face-background 'telega-msg-heading "#ede5c3")
  (x/define-keys telega-msg-button-map
                 '(("d" scroll-up-command)
                   ("e" scroll-down-command)
                   ("f" x/link-hint-open-in-current-window)
                   ("D" telega-msg-delete-marked-or-at-point)
                   ("E" telega-msg-edit)
                   ("F" telega-msg-forward-marked-or-at-point)
                   ("q" quit-window))))

;;; completing
(add-hook 'telega-chat-mode-hook 'z/telega-company)

(defun z/telega-company ()
  (set (make-local-variable 'company-backends)
       (append (list 'telega-emoji-company-backend
                     'telega-company-username
                     'telega-company-hashtag
                     'telega-company-markdown-precode)
               (when (telega-chat-bot-p telega-chatbuf--chat)
                 '(telega-company-botcmd))))
  (company-mode 1)
  (setq-local lsp-bridge-mode -1)
  (setq-local corfu-mode -1))

(provide 'x-telega)
;;; x-telega.el ends here
