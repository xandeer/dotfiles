;;; x-slack.el --- slack -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(x/package-use 'slack)

(setq slack-prefer-current-team t)

(slack-register-team
 :name "xmind"
 :token (auth-source-pick-first-password
         :host "xmind-hq.slack.com"
         :user "kevin@xmind.org")
 :cookie (auth-source-pick-first-password
          :host "xmind-hq.slack.com"
          :user "kevin@xmind.org^cookie")
 :subscribed-channels '((general)))

(provide 'x-slack)
;;; x-slack.el ends here
