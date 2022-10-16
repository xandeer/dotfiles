;;; x-insert-translated-text.el --- insert translated text -*- lexical-binding: t -*-
;;; Commentary:

;; Mostly copied from https://github.com/manateelazycat/insert-translated-name, but use api call instead of puppeteer.  Therefore, you need to provide your own DeepL api auth key.

;;; Code:

(require 'request)
(require 'json)

(defcustom x/insert-translated-text-deepl-key nil
  "Auth key of DeepL api."
  :type 'string
  :group 'x/insert-translated-text)

(defcustom x/insert-translated-text-deepl-pro? nil
  "Is your DeepL key a pro version?"
  :type 'boolean)

(defcustom x/insert-translated-text-activate-default-input-method? t
  "Whether to activate the default input method before typing."
  :type 'boolean
  :group 'x/insert-translated-text)

(defcustom x/insert-translated-text-target-lang "EN"
  "Target language.
EN: English
ZH: Chinese
DE: German
FR: French
IT: Italian
JA: Japanese
ES: Spanish
NL: Dutch
PL: Polish
PT: Portuguese
RU: Russian"
  :type 'string)

(defcustom x/insert-translated-text-end-symbol "|"
  "The end symbol of typing."
  :type 'string)

(defface x/insert-translated-text-font-lock-mark-text
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'x/insert-translated-text)


(defun x/insert-translated-text ()
  "Automatic insertion of translated content using DeepL."
  (interactive)
  (unless x/insert-translated-text-deepl-key
    (user-error "You need to provide an auth-key"))
  (x--insert-translated-text-activate))

(defun x--insert-translated-text-activate ()
  ;; Try to activate the defalut input method.
  (when x/insert-translated-text-activate-default-input-method?
    (set (make-local-variable 'insert-translated-text-need-deactivate-dim?) (not (string= current-input-method default-input-method)))
    (activate-input-method default-input-method))

  ;; Add monitor hook.
  (add-hook 'after-change-functions #'x--insert-translated-text-monitor-after-change nil t)
  (advice-add 'keyboard-quit :before #'x/insert-translated-text-inactivate)

  ;; Make sure build hash to contain placeholder.
  (unless (boundp 'insert-translated-text-placeholder-hash)
    (set (make-local-variable 'insert-translated-text-placeholder-hash) (make-hash-table :test 'equal)))

  ;; Make sure clean active overlay first.
  (when (and (boundp 'insert-translated-text-active-overlay)
             insert-translated-text-active-overlay)
    (delete-overlay insert-translated-text-active-overlay))

  ;; Reset active local variables
  (set (make-local-variable 'insert-translated-text-active-point) (point))
  (set (make-local-variable 'insert-translated-text-active-overlay) (make-overlay (point) (point)))

  ;; Activate new overlay from current point.
  (overlay-put insert-translated-text-active-overlay 'face 'x/insert-translated-text-font-lock-mark-text)

  ;; Print play hint.
  (unless (minibuffer-window-active-p (get-buffer-window))
    (message "Type and press \"%s\" to translate." x/insert-translated-text-end-symbol)))

(defun x/insert-translated-text-inactivate ()
  (interactive)
  ;; Disable input method if user has load it.
  (when (and (boundp 'insert-translated-text-need-deactivate-dim?)
             insert-translated-text-need-deactivate-dim?)
    (set (make-local-variable 'insert-translated-text-need-deactivate-dim?) nil)
    (deactivate-input-method))

  ;; Delete active overlay.
  (when (and (boundp 'insert-translated-text-active-overlay)
             insert-translated-text-active-overlay)
    (delete-overlay insert-translated-text-active-overlay))

  ;; Clean active local variables.
  (set (make-local-variable 'insert-translated-text-active-point) nil)
  (when (and (boundp 'insert-translated-text-active-overlay)
             insert-translated-text-active-overlay)
    (set (make-local-variable 'insert-translated-text-active-overlay) nil))

  (advice-remove 'keyboard-quit #'x/insert-translated-text-inactivate))

(defun x--insert-translated-text-monitor-after-change (start end len)
  (when (and (boundp 'insert-translated-text-active-point))
    (if insert-translated-text-active-point
        (let ((translate-start insert-translated-text-active-point)
              (translate-end (point)))
          (cond
           ;; Translate current text after press `x/insert-translated-text-end-symbol'.
           ((string-equal (buffer-substring-no-properties start end) x/insert-translated-text-end-symbol)
            (let ((text (buffer-substring-no-properties translate-start (1- translate-end))))
              ;; Delete input text.
              (kill-region translate-start translate-end)

              ;; Query translation.
              (x--insert-translated-text-query-translation text)

              ;; Inactivate.
              (x/insert-translated-text-inactivate)))
           ;; Update active overlay bound if user press any other non `x/insert-translated-text-end-symbol' character.
           (t
            (move-overlay insert-translated-text-active-overlay translate-start translate-end)))))))
;;

(defun x--insert-translated-text-generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (float-time))))

(defun x--insert-translated-text-query-translation (text)
  (if (string-equal text "")
      (message "Nothing input, cancel translate.")
    (let ((placeholder (x--insert-translated-text-generate-uuid)))
      ;; Store placeholder in hash.
      ;; bug: `insert-translated-text-placeholder-hash' is not initialized
      ;; thus I add such fix

      (unless (boundp 'insert-translated-text-placeholder-hash)
        (set (make-local-variable 'insert-translated-text-placeholder-hash) (make-hash-table :test 'equal)))

      (puthash placeholder (point) insert-translated-text-placeholder-hash)

      ;; Query translation.
      (x--insert-translated-text-request-translation text (buffer-name) placeholder))))

(defun x--insert-translated-text-request-translation (text buffername placeholder)
  (request
    (concat "https://api"
            (unless x/insert-translated-text-deepl-pro? "-free")
            ".deepl.com/v2/translate")
    :type "POST"
    :data `(("auth_key" . ,x/insert-translated-text-deepl-key)
            ("text" . ,text)
            ("target_lang" . ,x/insert-translated-text-target-lang))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((translation (string-join (mapcar 'cdadr (cdar data)) "\n")))
                  (x--insert-translated-text-update-translation-in-buffer text translation buffername placeholder))))))

(defun x--insert-translated-text-update-translation-in-buffer (text translation insert-buffer placeholder)
  (save-excursion
    (with-current-buffer insert-buffer
      (let ((placeholder-point (gethash placeholder insert-translated-text-placeholder-hash)))
        (if placeholder-point
            (progn
              ;; Insert result at placeholder point .
              (goto-char placeholder-point)
              (insert translation)

              ;; Remove placeholder from hash.
              (remhash placeholder insert-translated-text-placeholder-hash))
          (message (format "Something wrong that we can't found placeholder for %s: %s" text translation)))))))

(provide 'x-insert-translated-text)
;;; x-insert-translated-text.el ends here
