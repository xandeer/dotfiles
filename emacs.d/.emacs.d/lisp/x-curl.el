;;; x-curl.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x/fetch-api-as-raw-string (url)
  "Fetch the content of an API URL and return it as a raw string."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "\n\n")          ; Skip HTTP headers
    (buffer-substring (point) (point-max))))

(defun x/fetch-api-as-json (url)
  "Fetch the content of an API URL and return it as a json."
  (json-read-from-string (x/fetch-api-as-raw-string url)))

(defun get-json-value (key json-object)
  "Get the value associated with KEY in JSON-OBJECT."
  (cdr (assoc key json-object)))

(provide 'x-curl)
;;; x-curl.el ends here
