;;; x-wakatime.el --- x-wakatime -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'wakatime-mode t)
(setq wakatime-cli-path (expand-file-name "/opt/homebrew/bin/wakatime-cli"))

;; override default wakatime-call for errcode 112
(defun wakatime-call (savep)
  "Call WakaTime command."
  (let*
      (
       (command (wakatime-client-command savep))
       (process
        (start-process
         "Shell"
         (generate-new-buffer " *WakaTime messages*")
         shell-file-name
         shell-command-switch
         command)))

    (set-process-sentinel process
                          `(lambda (process signal)
                             (when (memq (process-status process) '(exit signal))
                               (kill-buffer (process-buffer process))
                               (let ((exit-status (process-exit-status process)))
                                 (when (and (not (= 0 exit-status)) (not (= 102 exit-status)))
                                   (when wakatime-disable-on-error
                                     (wakatime-mode -1)
                                     (global-wakatime-mode -1))
                                   (cond
                                    ((= exit-status 103) (error "WakaTime Error (%s) Config file parse error. Check your ~/.wakatime.cfg file." exit-status))
                                    ((= exit-status 104) (error "WakaTime Error (%s) Invalid API Key. Set your api key with: (custom-set-variables '(wakatime-api-key \"XXXX\"))" exit-status))
                                    ((= exit-status 105) (error "WakaTime Error (%s) Unknown wakatime-cli error. Please check your ~/.wakatime.log file and open a new issue at https://github.com/wakatime/wakatime-mode" exit-status))
                                    ((= exit-status 106) (error "WakaTime Error (%s) Malformed heartbeat error. Please check your ~/.wakatime.log file and open a new issue at https://github.com/wakatime/wakatime-mode" exit-status))
                                    ((= exit-status 112) (message "WakaTime Error (%s) Cli" exit-status))
                                    (t (error "WakaTime Error (%s) Make sure this command runs in a Terminal: %s" exit-status (wakatime-client-command nil)))))))))
    (set-process-query-on-exit-flag process nil)))

(global-wakatime-mode)

(provide 'x-wakatime)
;;; x-wakatime.el ends here
