;;; azure-tts.el --- Azure TTS client for Emacs

;;; Commentary:

(defvar azure-tts-last-text nil "The cache of the last text.")

(defun azure-tts-replay ()
  "Play the last result."
  (interactive)
  (when azure-tts-last-text
    (x/start-process (format "tts %s" azure-tts-last-text))))

(defun azure-tts-play-region (start end)
  "Play the region from START to END."
  (interactive "r")
  (let ((text (x/text-normalize (filter-buffer-substring start end))))
    (setq azure-tts-last-text text)
    (x/start-process (format "tts %s" text))))

(provide 'azure-tts)
;;; azure-tts.el ends here
