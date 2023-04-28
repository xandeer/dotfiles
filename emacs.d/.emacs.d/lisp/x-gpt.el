;;; x-gpt.el --- gpt -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'request)
(setq request-backend 'curl)

(request
  "https://api.example.com/data"
  :type "POST"
  :headers '(("Content-Type" . "application/json"))
  :data (json-encode '((key . "value") (key2 . "value2")))
  :parser 'json-read
  :success (cl-function
            (lambda (&key data &allow-other-keys)
              (message "Posted data: %S" data)))
  :error (cl-function
          (lambda (&rest args &key error-thrown &allow-other-keys)
            (message "Error: %S" error-thrown))))

(request
 "https://api.example.com/data"
 :type "GET"
 :headers '(("Content-Type" . "application/json"))
 :params '(("key" . "value") ("key2" . "value2"))
 :parser 'json-read
 :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Fetched data: %S" data)))
 :error (cl-function
          (lambda (&rest args &key error-thrown &allow-other-keys)
            (message "Error: %S" error-thrown))))

(provide 'x-gpt)
;;; x-gpt.el ends here
