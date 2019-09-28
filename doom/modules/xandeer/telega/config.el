;;; xandeer/ui/config.el -*- lexical-binding: t; -*-

(use-package! telega
  :commands (telega)
  :defer t
  :hook (telega-chat-mode . doom-mark-buffer-as-real-h)
  :config
  (telega-mode-line-mode 1)
  (set-popup-rule! "^\\*Telega Root" :side 'left :size 0.2 :quit nil :select t)
  (set-popup-rule! "^◀\\[.*@.*\\]" :side 'right :size 0.6 :quit nil :select t)
  (setq ;telega-use-tracking t
        ;; telega-known-inline-bots '("@")
   telega-proxies (list '(:sever "127.0.0.1" :port 8010 :enable t :type (:@type "proxyTypeHttp")))
        telega-sticker-set-download t
        telega-chat-button-width 28
        telega-root-fill-column 48)
  (when (featurep! :completion ivy)
    (load! "+ivy"))
  (when (featurep! :completion company)
    (add-hook 'telega-chat-mode-hook
              (lambda ()
                (set (make-local-variable 'company-backends)
                     (append '(telega-company-emoji
                               telega-company-username
                               telega-company-hashtag)
                             (when (telega-chat-bot-p telega-chatbuf--chat)
                               '(telega-company-botcmd)))))))
  (when (featurep! :editor evil)
    (map!
     (:map telega-msg-button-map
       "k" nil
       "l" nil)))
  (when (eq window-system 'mac)
    ;; emacs-mac have some bug on user avatars
    (setq telega-user-use-avatars nil)))
