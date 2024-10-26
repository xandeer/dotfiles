;;; x-sh.el --- x-sh -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; async commands
(defvar x/sh-async-command-history nil
  "The history list for async shell commands.")

(defun x/sh-exec-async ()
  "Execute a shell command asynchronously."
  (interactive)
  (let ((command (completing-read "Command: "
                                  x/sh-async-command-history
                                  nil nil nil
                                  'x/sh-async-command-history)))
    (x/start-process command t)))

(x/define-keys global-map
               '(("H-r" x/sh-exec-async)))

(defvar x/sh-adb-history nil
  "The history list for adb connect functions.")

(defun x/sh-adb-connect ()
  "Connect to an android device."
  (interactive)
  (let ((ip (completing-read "Adb Connect: "
                             x/sh-adb-history
                             nil nil
                             (concat (s-join "." (butlast (s-split "\\." (x/ifconfig)))) ".")
                             'x/sh-adb-history)))
    (x/start-process
     (concat "adb connect " ip))))

(defvar x/open-localhost-history nil
  "The history list for open localhost functions.")

(defun x/open-localhost ()
  "Open localhost."
  (interactive)
  (let ((ip (completing-read "Open: "
                             x/open-localhost-history
                             nil nil
                             (x/ifconfig)
                             'x/open-localhost-history)))
    (x/open (concat "http://" ip))))

(autoload 'osx-lib-get-from-clipboard "osx-lib")
(defun x/sh-run-clipboard ()
  "Call `x/start-process' with the clipboard content."
  (interactive)
  (x/start-process (osx-lib-get-from-clipboard)))

(provide 'x-sh)
;;; x-sh.el ends here
