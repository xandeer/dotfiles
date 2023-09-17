;;; x-utils.el --- x-utils -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; replace
(defun x/replace (old new &optional beg end)
  "Replace occurrences of string OLD with string NEW in the current buffer.
Operate between positions BEG and END.
If BEG and END are not provided, the function operates on the entire buffer."
  ;; Set BEG and END to the buffer boundaries if they are not provided
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))

  ;; Ensure BEG is less than or equal to END
  (when (> beg end)
    (let (mid)
      (setq mid end end beg beg mid)))
  ;; Save the current position and restore it later
  (save-excursion
    ;; Go to the END position
    (goto-char end)
    ;; Insert the modified text
    (insert
     ;; Create a temporary buffer to perform the replacement
     (let ((buf (current-buffer)))
       (with-temp-buffer
         ;; Switch to the temporary buffer and copy the text from the original buffer
         (switch-to-buffer (current-buffer) nil t)
         (insert-buffer-substring buf beg end)
         ;; Perform the replacement in the temporary buffer
         (goto-char (point-min))
         (while (re-search-forward old nil t)
           (replace-match new))
         ;; Return the modified text from the temporary buffer
         (buffer-string))))
    ;; Delete the original text between BEG and END
    (delete-region beg end)))

(defun x/convert-to-chinese-quotations ()
  "Convert all [“|“ ‘|’] to [ 「|」『|』] in current buffer."
  (interactive)

  (let ((quotas
         '(("‘" . "『")
           ("’" . "』")
           ("“" . "「")
           ("”" . "」")
           (", " . "，")
           ("," . "，")
           ("; " . "；")
           ("\\. " . "。")
           ("^\\([0-9]*\\)。" . "\\1. ")
           ;; sub items
           ("\\(  [0-9]*\\)。" . "\\1. ")
           ("\\.」" . "。」")
           ("\\.』" . "。』")
           ("\\.$" . "。")
           ("? " . "？")
           ;; (": " . "：")
           (" \\{2,\\}:" . " :")
           ("（" . "(")
           ("）" . ")")
           ("・" . "·")
           ("! " . "！")
           (" )" . ")")
           (" 」" . "」")
           (" 』" . "』"))))
    (mapc (lambda (q)
            (x/replace (car q) (cdr q)))
          quotas)))

(defun x/rm-zlib-suffix ()
  "Remove zlib suffix in the current Dired buffer."
  (interactive)
  (wdired-change-to-wdired-mode)
  (replace-string " (z-lib.org)" "")
  (replace-string " (Z-Library)" "")
  (wdired-finish-edit))

(defun x/flush-double-newlines ()
  "Replace double newlines with one."
  (interactive)
  (save-excursion
    (replace-regexp "\n\n\n" "\n\n")))

(defun x/delete-current-buffer ()
  "Delete the current buffer."
  (interactive)
  (delete-file (buffer-name))
  (kill-current-buffer))

(defun x/duplicate-line ()
  "Duplicate the current line."
  (interactive)
  (progn
    (move-beginning-of-line 1)
    (insert (thing-at-point 'line))
    (move-end-of-line 1)))

(defun x/bookmark (name)
  "Goto bookmark with NAME, or update it."
  (interactive)
  (if (s-contains? name (buffer-name))
      (bookmark-set name)
    (bookmark-jump name)))

(defun x/kill-other-window-buffer ()
  "Kill the buffer in other window."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

(defun x/expand-repo (path)
  "Expand PATH in ~/prejects/personal ."
  (expand-file-name path "~/projects/personal"))

(defun x/expand-note (path)
  "Expand PATH in `org-directory`."
  (expand-file-name path (x/expand-repo "notes")))

(global-set-key (kbd "H-e") 'x/kill-other-window-buffer)
(global-set-key (kbd "H-b") (lambda () (interactive) (switch-to-buffer "*scratch*")))

(defun x/trash-temp ()
  "Move some temp files to trash."
  (interactive)
  (dolist (path '("~/syncthing/donut/apk"
                  "~/syncthing/personal/temp"))
    (shell-command (concat "trash " path
                           "; mkdir -p " path)
                   "*x/trash-temp*")))

;; Move it to /Library/LaunchDaemons
;; (add-hook #'after-init-hook
          ;; (lambda ()
            ;; (async-shell-command "~/bin/hs -d ~/Downloads" "*hs-daemon*")))

;; (defun x/change-hs-root (path)
;;   (interactive)
;;   (let ((url-request-method "PUT"))
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          (concat "http://localhost"
;;                  (expand-file-name path)))
;;       (buffer-string))))

(defun x/change-hs-root (path)
  "Start dufs at PATH."
  (interactive)
  (let* ((program "hs")
         (buffer (x/process-buffer-get program))
         (process (get-buffer-process buffer)))
    (when process
      (kill-process process)
      (sleep-for 0.1))
    (x/start-process (format "%s %s" program path))))

(defun x/change-hs-on-dired ()
  (interactive)
  (x/change-hs-root dired-directory))

(defun x/ifconfig ()
  (interactive)
  (message (format-network-address
            (or
             (car (network-interface-info "en0"))
             (car (network-interface-info "en1")))
            t)))

;;; file
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
(defun x/rename ()
  "Renames both current buffer and file it's visiting to a new name."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (when (file-exists-p filename)
      (let ((new-name (read-from-minibuffer "New name: " name)))
        (rename-file filename new-name 1)
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))

(defun x/browse-current-file ()
  "Open the current file as a URL using `browse-url`."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun x/copy-file-path ()
  "Copy the current buffer's file path to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun x/string-append-time-suffix (string)
  "Append a time suffix to STRING."
  (concat string (format-time-string "%F-%H-%M-%S%3N")))

(defun x/launch-separate-emacs-under-x ()
  "Launch a separate Emacs instance under X."
  (interactive)
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun x/restart-emacs ()
  "Restart Emacs."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook (list #'x/launch-separate-emacs-under-x))))
    (save-buffers-kill-emacs)))

(defun x/load-current ()
  "Load the current elisp file."
  (interactive)
  (load-file (buffer-file-name)))

(defun x/toggle-narrow (arg)
  (interactive "p")
  (if (buffer-narrowed-p)
      (widen)
    (cond ((region-active-p)
           (narrow-to-region (region-beginning) (region-end)))
          (lispy-mode
           (lispy-narrow arg))
          ((equal major-mode 'org-mode)
           (org-toggle-narrow-to-subtree))
          (smartparens-mode
           (sp-narrow-to-sexp arg)))))

(defun x/new-line-before ()
  "Insert a new line before the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun x/new-line-after ()
  "Insert a new line after the current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

;;; exercism
(defun x/exercism-submit ()
  "Submit the current buffer to Exercism."
  (interactive)
  (shell-command (format "exercism submit %s"
                         (buffer-file-name))))

(defun x/exercism-open-readme-other-window ()
  "Open the README related with current file."
  (interactive)
  (let ((root (locate-dominating-file (buffer-file-name) "README.md")))
    (find-file-other-window (expand-file-name "README.md" root))))

;;; cow
(defun x--cow (file)
  "Upload the FILE to cow."
  (x/start-process (format "%s %s"
                           (expand-file-name "~/bin/cow")
                           file)))

(defun x/cow-current ()
  "Upload the current file to cow."
  (interactive)
  (x--cow
   (if (equal major-mode 'dired-mode) (dired-get-filename)
     (buffer-file-name))))

(defun x/cow ()
  "Read a file and then upload to cow."
  (interactive)
  (x--cow
   (read-file-name "Cow Upload: " (expand-file-name "~/syncthing/"))))

(defun x/null-st ()
  "Upload the current file to 0x0.st"
  (interactive)
  (x/start-process
   (format "null.exs %s"
           (buffer-file-name))))

;;; dots
(defun x/makefile-executor-execute (filename)
  "Execute a Makefile target from FILENAME.

FILENAME defaults to current buffer."
  (let ((target (makefile-executor-select-target filename)))
    (makefile-executor-store-cache filename target)
    (compile (format "make -f %s -C %s %s"
                     (shell-quote-argument filename)
                     (shell-quote-argument (file-name-directory filename))
                     target))))

(autoload 'makefile-executor-select-target "makefile-executor")

(defun x/dots ()
  "Run make with dotfiles/Makefile."
  (interactive)
  (x/makefile-executor-execute (expand-file-name "~/projects/personal/dotfiles/Makefile")))

;;; string
;; copy from https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el
;; Like diminish, but for major modes
(defun x/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(autoload 'derived-mode-hook-name "derived")
(defun x/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'x/set-major-mode-name name)))

;;; keybindings
(defun x/define-keys (map bindings)
  "Define keys in MAP according to BINDINGS.

Example:
  (x/define-keys org-agenda-mode-map
                 (quote ((\"k\" org-agenda-previous-item)
                         (\"p\" org-agenda-previous-item)
                         (\"n\" org-agenda-next-item)
                         (\"j\" org-agenda-next-item)
                         (\"T\" org-agenda-goto-today)
                         (\"i\" org-agenda-clock-in)
                         (\"o\" org-agenda-clock-goto)
                         ([remap org-schedule] x/org-schedule)))"
  (mapc (lambda (binding)
          (let* ((key (car binding))
                 (key (if (stringp key)
                          (kbd key)
                        key))
                 (command (cadr binding)))
            (define-key map key command)))
        bindings))

;;; url
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

(defun x/get-current-location ()
  "Fetch current latitude and longitude using an IP-based geolocation service."
  (interactive)
  (let ((url "https://ipinfo.io/json"))
    (get-json-value 'loc (x/fetch-api-as-json url))))

(defvar x/location nil)

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

    (insert (format "%s, %d°C, fl: %d°C, %d%%, %dkm/h, %dkm"
                    condition temp feel humidity wind vis))))

(provide 'x-utils)
;;; x-utils.el ends here
