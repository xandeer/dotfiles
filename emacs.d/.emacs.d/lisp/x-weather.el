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

(defun x/get-weather-data (location)
  "Fetch and parse weather data for LOCATION from weatherapi.com.
Returns an alist containing relevant weather information."
  (let* ((api-key (auth-source-pick-first-password
                   :host "weatherapi.com"
                   :user "weather"))
         (url (format "https://api.weatherapi.com/v1/current.json?key=%s&q=%s"
                      api-key location))
         (response (x/fetch-api-as-json url)))
    (when response
      (let ((current (get-json-value 'current response)))
        (list
         (cons 'condition (get-json-value 'text (get-json-value 'condition current)))
         (cons 'temperature (get-json-value 'temp_c current))
         (cons 'feels-like (get-json-value 'feelslike_c current))
         (cons 'humidity (get-json-value 'humidity current))
         (cons 'wind-speed (get-json-value 'wind_kph current))
         (cons 'visibility (get-json-value 'vis_km current)))))))

(defun x/format-weather-string (weather-data)
  "Format WEATHER-DATA into a user-friendly string.
WEATHER-DATA is an alist returned by `x/get-weather-data`."
  (let ((condition (alist-get 'condition weather-data))
        (temperature (alist-get 'temperature weather-data))
        (feels-like (alist-get 'feels-like weather-data))
        (humidity (alist-get 'humidity weather-data))
        (wind-speed (alist-get 'wind-speed weather-data))
        (visibility (alist-get 'visibility weather-data)))
    (format ", %s, %d°C, fl: %d°C, %d%%, %dkm/h, %dkm, "
            condition temperature feels-like humidity wind-speed visibility)))

;;;###autoload
(defun x/insert-weather ()
  "Insert the current weather for the user's location into the buffer.
Uses weatherapi.com to fetch weather data."
  (interactive)
  ;; Ensure location is set
  (unless x/location
    (setq x/location (x/get-current-location)))

  ;; Fetch weather data
  (let ((weather-data (x/get-weather-data x/location)))
    (if weather-data
        ;; Insert formatted weather data into the buffer
        (insert (x/format-weather-string weather-data))
      ;; Notify user if fetching weather data failed
      (message "Unable to fetch weather data for location: %s" x/location))))

(provide 'x-weather)
;;; x-weather.el ends here
