;;; x-weather.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'x-curl)

(defvar x/location nil)

;;;###autoload
(defun x/get-current-location ()
  "Fetch current latitude and longitude using an IP-based geolocation service."
  (interactive)
  (let ((url "https://ipinfo.io/json"))
    (get-json-value 'loc (x/fetch-api-as-json url))))

;;;###autoload
(defun x/insert-weather ()
  "Insert current weather by weatherapi.com."
  (interactive)
  (unless x/location
    (setq x/location (x/get-current-location)))

  (let* ((url (format
               "https://api.weatherapi.com/v1/current.json?key=%s&q=%s"
               (auth-source-pick-first-password
                :host "weatherapi.com"
                :user "weather")
               x/location))
         (current (get-json-value 'current (x/fetch-api-as-json url)))

         (condition (get-json-value 'text (get-json-value 'condition current)))
         (temp (get-json-value 'temp_c current))
         (feel (get-json-value 'feelslike_c current))
         (humidity (get-json-value 'humidity current))
         (wind (get-json-value 'wind_kph current))
         (vis (get-json-value 'vis_km current)))

    (insert (format ", %s, %d°C, fl: %d°C, %d%%, %dkm/h, %dkm"
                    condition temp feel humidity wind vis))))

(provide 'x-weather)
;;; x-weather.el ends here
