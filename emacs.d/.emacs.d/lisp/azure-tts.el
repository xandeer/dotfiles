;;; azure-tts.el --- Azure TTS client for Emacs

;;; Commentary:

;; This package provides a client for Azure TTS service.
;; You can use it to synthesize speech from text.
;; First you should register an Azure TTS service and get the auth key.
;; After that, you should store the key in your authinfo file:
;;
;; machine tts.speech.microsoft.com login azure-tts password <your-key>
;;
;; Then you should customize the `azure-tts-region' as your Azure TTS service
;; registered in.
;; Now you can use `azure-tts-play-region' to synthesize speech from the
;; selected region.
;;
;; `azure-tts' will use `azure-tts--ffplay' to play the downloaded audio file.
;; You can customize `azure-tts-player-function' to use other player.
;; Currently `azure-tts--afplay' (for macOS user) is provided.

;;; Code:

(require 'subr-x)
(require 'auth-source)
(require 'rx)

;;; Customization

(defgroup azure-tts nil
  "Azure TTS."
  :group 'azure)

(defcustom azure-tts-region "eastus"
  "Azure TTS region."
  :type 'string
  :group 'azure-tts)

(setq azure-tts-region "eastasia")

(defcustom azure-tts-temp-dir nil
  "Azure TTS temporary dir for storing downloaded files.
Will use `make-temp-file' to create a temporary directory if nil."
  :type 'string
  :group 'azure-tts)

(defcustom azure-tts-player-function #'azure-tts--ffplay
  "Function to play the downloaded audio file.
The function should accept one argument, the path of the audio file."
  :type 'function
  :group 'azure-tts)

(setq azure-tts-player-function #'azure-tts--afplay)

;; https://speech.microsoft.com/portal/voicegallery
(defcustom azure-tts-zh-voice-name "zh-CN-XiaochenNeural"
  "Default zh cn voice name.")
;; (setq azure-tts-zh-voice-name "zh-CN-YunxiNeural")
;; (setq azure-tts-zh-voice-name "zh-CN-YunyeNeural")

(defcustom azure-tts-en-voice-name "en-US-MichelleNeural"
  "Default en us voice name.")

(defcustom azure-tts-default-rate 0
  "Default rate."
  :type 'integer
  :group 'azure-tts)

(setq azure-tts-default-rate -10)

(defcustom azure-tts-default-pitch 0
  "Default pitch."
  :type 'integer
  :group 'azure-tts)

;;; Storage

(defvar azure-tts--temp-dir nil)

(defun azure-tts--download-file-dir ()
  "Return the directory to store the downloaded file."
  (if (null azure-tts--temp-dir)
      (setq azure-tts--temp-dir
            (if (null azure-tts-temp-dir)
                (make-temp-file "azure_tts_" t)
              azure-tts-temp-dir))
    azure-tts--temp-dir))

(defun azure-tts--download-file-path (query)
  "Return the path for storing the QUERY results."
  (let ((hash (secure-hash 'sha256 query)))
    (expand-file-name hash (azure-tts--download-file-dir))))

;;; APIs

(defvar azure-tts--api-host-template
  "https://%s.tts.speech.microsoft.com/cognitiveservices/v1"
  "Base URL for Azure TTS.")

(defun azure-tts-key ()
  "Get auth key from authinfo."
  (interactive)
  (auth-source-pick-first-password
   :host "tts.speech.microsoft.com" :user "azure-tts"))

(defun azure-tts--api-host (region)
  "Azure TTS API host.
REGION is the region your Azure TTS service registered in."
  (format azure-tts--api-host-template region))

;; Query

(defvar azure-tts--ssml-template
  "<speak version=\"1.0\" xmlns=\"http://www.w3.org/2001/10/synthesis\" xml:lang=\"en-US\">
  <voice name=\"%s\">
    <prosody rate=\"%s%%\" pitch=\"%s%%\">
      %s
    </prosody>
  </voice>
</speak>" "SSML query string.")

(defun azure-tts--ssml (text voice-name rate pitch)
  "SSML query string.
TEXT is the text to be synthesized.
RATE is the percentage of the default speaking rate,
nagative value for slower, positive value for faster.
PITCH is the percentage of the default pitch.
And you can find the list of supported VOICE-NAME at:
https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/language-support#text-to-speech."
  (format
   azure-tts--ssml-template
   (string-trim voice-name)
   rate
   pitch
   (string-trim text)))

;;; Download

(defun azure-tts--download-audio (ssml callback)
  "Download the audio file of the SSML query.
CALLBACK is a function with downloaded file path as the argument,
which will be called after the download is finished."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/ssml+xml")
            ("X-Microsoft-OutputFormat" . "audio-48khz-192kbitrate-mono-mp3")
            ("Ocp-Apim-Subscription-Key" .
             ,(encode-coding-string (azure-tts-key) 'utf-8))))
         (url-request-data (encode-coding-string ssml 'utf-8))
         (url (azure-tts--api-host azure-tts-region))
         (download-path (concat
                         (azure-tts--download-file-path url-request-data)
                         ".mp3")))
    (if (file-exists-p download-path)
        (funcall callback download-path)
      (url-retrieve
       url
       (lambda (_status path cb)
         (goto-char (point-min))
         (if (not (string-match "200 OK" (buffer-string)))
             (message "Error: %s" (buffer-string))
           (let ((inhibit-message t))
             (re-search-forward "\r?\n\r?\n")
             (write-region (point) (point-max) path 0))
           (funcall cb path)))
       (list download-path callback)
       'silent))))

;;; Play

(defun azure-tts--ffplay (path)
  "Play the audio file at PATH use ffplay."
  (start-process "azure-tts" nil "ffplay" "-nodisp" "-autoexit" path))

(defun azure-tts--afplay (path)
  "Play the audio file at PATH use afplay."
  (start-process "azure-tts" nil "afplay" path))

(defvar azure-tts--play-process nil)

(defun azure-tts--kill-play-process ()
  "Kill the process if it's still running."
  (when (and azure-tts--play-process
             (process-live-p azure-tts--play-process))
    (kill-process azure-tts--play-process)))

(defun azure-tts--player-play (path)
  "Play the audio file at PATH."
  (azure-tts--kill-play-process)
  (setq azure-tts--play-process
        (funcall azure-tts-player-function path)))

(defun azure-tts-play (text voice-name rate pitch)
  "Play the TEXT with the given VOICE-NAME, RATE and PITCH."
  (azure-tts--download-audio
   (azure-tts--ssml text voice-name rate pitch)
   #'azure-tts--player-play))

(defun azure-tts-play-region (start end &optional voice-name rate pitch)
  "Play the region from START to END with the given VOICE-NAME, RATE and PITCH.
If VOICE-NAME, RATE and PITCH are not given, use the default values.
\\[universal-argument] means decrease the rate by 20."
  (interactive "r")
  (let ((rate-decrease
         (if (and current-prefix-arg (listp current-prefix-arg))
             (round (* 20 (log (car current-prefix-arg) 4)))
           0)))
    (azure-tts-play
     (azure-tts--get-plain-text start end)
     (or voice-name azure-tts-en-voice-name)
     (- (or rate azure-tts-default-rate)
        rate-decrease)
     (or pitch azure-tts-default-pitch))))

(defun azure-tts--get-plain-text (start end)
  "Extract plain text from the current buffer between START and END.
Remove Org-mode links, asterisks, and dynamic block markers from the text."
  (interactive "r")
  ;; Define the regular expressions for Org-mode links, asterisks,
  ;; and dynamic block markers
  (let* ((org-link (rx (seq "[[" (*? anything) "][" (group (+? anything)) "]]")))
         (asterisk (rx (any "*")))
         (org-dblock (rx (seq "#+" (group (| "BEGIN" "begin" "END" "end")) (0+ nonl)))))
    ;; Use filter-buffer-substring to get the text between START and END
    ;; Replace Org-mode links with their description using replace-regexp-in-string
    ;; Remove dynamic block markers and asterisks using replace-regexp-in-string
    (->> (filter-buffer-substring start end)
         (replace-regexp-in-string org-link "\\1")
         (replace-regexp-in-string org-dblock "")
         (replace-regexp-in-string asterisk ""))))

(defun azure-tts-play-region-chinese (start end &optional rate pitch)
  "Play the region from START to END in Chinese with the given RATE and PITCH.
If RATE and PITCH are not given, use the default values.
\\[universal-argument] means decrease the rate by 20."
  (interactive "r")
  (azure-tts-play-region start end azure-tts-zh-voice-name rate pitch))

(defun azure-tts-play-region-english (start end &optional rate pitch)
  "Play the region from START to END in Chinese with the given RATE and PITCH.
If RATE and PITCH are not given, use the default values.
\\[universal-argument] means decrease the rate by 20."
  (interactive "r")
  (azure-tts-play-region start end azure-tts-en-voice-name rate pitch))

(provide 'azure-tts)
;;; azure-tts.el ends here
