;;; xandeer-core-buffer.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Core Buffer.

;;; Code:

;;;###autoload
(defvar xandeer/real-buffer-functions
  '(xandeer/dired-buffer-p)
  "A list of predicate functions run to determine if a buffer is real, unlike
`xandeer/unreal-buffer-functions'. They are passed one argument: the buffer to be
tested.
Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.
See `xandeer/real-buffer-p' for more information.")

;;;###autoload
(defvar xandeer/unreal-buffer-functions
  '(minibufferp xandeer/special-buffer-p xandeer/non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `xandeer/real-buffer-functions'. They are passed one argument: the buffer to
be tested.
Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.
See `xandeer/real-buffer-p' for more information.")

;;;###autoload
(defvar-local xandeer/real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`xandeer/real-buffer-p' for more information.")

;;;###autoload
(defvar xandeer/fallback-buffer-name "*scratch*"
  "The name of the buffer to fall back to if no other buffers exist (will create
it if it doesn't exist).")


;;
;; Functions

;;;###autoload
(defun xandeer/buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (xandeer/real-buffer-p buf)
      (eq buf (xandeer/fallback-buffer))))

;;;###autoload
(defun xandeer/fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `xandeer/fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create xandeer/fallback-buffer-name)))

;;;###autoload
(defalias 'xandeer/buffer-list #'buffer-list)

;;;###autoload
(defun xandeer/project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.
If PROJECT is nil, default to the current project.
If no project is active, return all buffers."
  (let ((buffers (xandeer/buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (xandeer/project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

;;;###autoload
(defun xandeer/open-projects ()
  "Return a list of projects with open buffers."
  (cl-loop with projects = (make-hash-table :test 'equal :size 8)
           for buffer in (xandeer/buffer-list)
           if (buffer-live-p buffer)
           if (xandeer/real-buffer-p buffer)
           if (with-current-buffer buffer (xandeer/project-root))
           do (puthash (abbreviate-file-name it) t projects)
           finally return (hash-table-keys projects)))

;;;###autoload
(defun xandeer/dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (with-current-buffer buf (derived-mode-p 'dired-mode)))

;;;###autoload
(defun xandeer/special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

;;;###autoload
(defun xandeer/temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

;;;###autoload
(defun xandeer/visible-buffer-p (buf)
  "Return non-nil if BUF is visible."
  (get-buffer-window buf))

;;;###autoload
(defun xandeer/buried-buffer-p (buf)
  "Return non-nil if BUF is not visible."
  (not (xandeer/visible-buffer-p buf)))

;;;###autoload
(defun xandeer/non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

;;;###autoload
(defun xandeer/real-buffer-list (&optional buffer-list)
  "Return a list of buffers that satify `xandeer/real-buffer-p'."
  (cl-remove-if-not #'xandeer/real-buffer-p (or buffer-list (xandeer/buffer-list))))

;;;###autoload
(defun xandeer/real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.
A real buffer is a useful buffer; a first class citizen in Doom. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.
The exact criteria for a real buffer is:
  1. A non-nil value for the buffer-local value of the `xandeer/real-buffer-p'
     variable OR
  2. Any function in `xandeer/real-buffer-functions' returns non-nil OR
  3. None of the functions in `xandeer/unreal-buffer-functions' must return
     non-nil.
If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (and (buffer-live-p buf)
         (not (xandeer/temp-buffer-p buf))
         (or (buffer-local-value 'xandeer/real-buffer-p buf)
             (run-hook-with-args-until-success 'xandeer/real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'xandeer/unreal-buffer-functions buf))))))

;;;###autoload
(defun xandeer/unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.
See `xandeer/real-buffer-p' for details on what that means."
  (not (xandeer/real-buffer-p buffer-or-name)))

;;;###autoload
(defun xandeer/buffers-in-mode (modes &optional buffer-list derived-p)
  "Return a list of buffers whose `major-mode' is `eq' to MODE(S).
If DERIVED-P, test with `derived-mode-p', otherwise use `eq'."
  (let ((modes (xandeer/enlist modes)))
    (cl-remove-if-not (if derived-p
                          (lambda (buf)
                            (with-current-buffer buf
                              (apply #'derived-mode-p modes)))
                        (lambda (buf)
                          (memq (buffer-local-value 'major-mode buf) modes)))
                      (or buffer-list (xandeer/buffer-list)))))

;;;###autoload
(defun xandeer/visible-windows (&optional window-list)
  "Return a list of the visible, non-popup (dedicated) windows."
  (cl-loop for window in (or window-list (window-list))
           when (or (window-parameter window 'visible)
                    (not (window-dedicated-p window)))
           collect window))

;;;###autoload
(defun xandeer/visible-buffers (&optional buffer-list)
  "Return a list of visible buffers (i.e. not buried)."
  (if buffer-list
      (cl-remove-if-not #'get-buffer-window buffer-list)
    (delete-dups (mapcar #'window-buffer (window-list)))))

;;;###autoload
(defun xandeer/buried-buffers (&optional buffer-list)
  "Get a list of buffers that are buried."
  (cl-remove-if #'get-buffer-window (or buffer-list (xandeer/buffer-list))))

;;;###autoload
(defun xandeer/matching-buffers (pattern &optional buffer-list)
  "Get a list of all buffers that match the regex PATTERN."
  (cl-loop for buf in (or buffer-list (xandeer/buffer-list))
           when (string-match-p pattern (buffer-name buf))
           collect buf))

;;;###autoload
(defun xandeer/set-buffer-real (buffer flag)
  "Forcibly mark BUFFER as FLAG (non-nil = real)."
  (with-current-buffer buffer
    (setq xandeer/real-buffer-p flag)))

;;;###autoload
(defun xandeer/kill-buffer-and-windows (buffer)
  "Kill the buffer and delete all the windows it's displayed in."
  (dolist (window (get-buffer-window-list buffer))
    (unless (one-window-p t)
      (delete-window window)))
  (kill-buffer buffer))

;;;###autoload
(defun xandeer/fixup-windows (windows)
  "Ensure that each of WINDOWS is showing a real buffer or the fallback buffer."
  (dolist (window windows)
    (with-selected-window window
      (when (xandeer/unreal-buffer-p (window-buffer))
        (previous-buffer)
        (when (xandeer/unreal-buffer-p (window-buffer))
          (switch-to-buffer (xandeer/fallback-buffer)))))))

;;;###autoload
(defun xandeer/kill-buffer-fixup-windows (buffer)
  "Kill the BUFFER and ensure all the windows it was displayed in have switched
to a real buffer or the fallback buffer."
  (let ((windows (get-buffer-window-list buffer)))
    (kill-buffer buffer)
    (xandeer/fixup-windows (cl-remove-if-not #'window-live-p windows))))

;;;###autoload
(defun xandeer/kill-buffers-fixup-windows (buffers)
  "Kill the BUFFERS and ensure all the windows they were displayed in have
switched to a real buffer or the fallback buffer."
  (let ((seen-windows (make-hash-table :test 'eq :size 8)))
    (dolist (buffer buffers)
      (let ((windows (get-buffer-window-list buffer)))
        (kill-buffer buffer)
        (dolist (window (cl-remove-if-not #'window-live-p windows))
          (puthash window t seen-windows))))
    (xandeer/fixup-windows (hash-table-keys seen-windows))))

;;;###autoload
(defun xandeer/kill-matching-buffers (pattern &optional buffer-list)
  "Kill all buffers (in current workspace OR in BUFFER-LIST) that match the
regex PATTERN. Returns the number of killed buffers."
  (let ((buffers (xandeer/matching-buffers pattern buffer-list)))
    (dolist (buf buffers (length buffers))
      (kill-buffer buf))))


;;
;; Hooks

;;;###autoload
(defun xandeer/mark-buffer-as-real-h ()
  "Hook function that marks the current buffer as real."
  (xandeer/set-buffer-real (current-buffer) t))


;;
;; Interactive commands

;;;###autoload
(defun xandeer/kill-this-buffer-in-all-windows (buffer &optional dont-save)
  "Kill BUFFER globally and ensure all windows previously showing this buffer
have switched to a real buffer or the fallback buffer.
If DONT-SAVE, don't prompt to save modified buffers (discarding their changes)."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (cl-assert (bufferp buffer) t)
  (when (and (buffer-modified-p buffer) dont-save)
    (with-current-buffer buffer
      (set-buffer-modified-p nil)))
  (xandeer/kill-buffer-fixup-windows buffer))


(defun xandeer/message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

;;;###autoload
(defun xandeer/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.
If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (xandeer/project-buffer-list)
           (xandeer/buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (xandeer/fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (xandeer/message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun xandeer/kill-other-buffers (&optional buffer-list interactive)
  "Kill all other buffers (besides the current one).
If the prefix arg is passed, kill only buffers that belong to the current
project."
  (interactive
   (list (delq (current-buffer)
               (if current-prefix-arg
                   (xandeer/project-buffer-list)
                 (xandeer/buffer-list)))
         t))
  (mapc #'xandeer/kill-buffer-and-windows buffer-list)
  (xandeer/message-or-count
   interactive "Killed %d other buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun xandeer/kill-matching-buffers (pattern &optional buffer-list interactive)
  "Kill buffers that match PATTERN in BUFFER-LIST.
If the prefix arg is passed, only kill matching buffers in the current project."
  (interactive
   (list (read-regexp "Buffer pattern: ")
         (if current-prefix-arg
             (xandeer/project-buffer-list)
           (xandeer/buffer-list))
         t))
  (xandeer/kill-matching-buffers pattern buffer-list)
  (when interactive
    (message "Killed %d buffer(s)"
             (- (length buffer-list)
                (length (cl-remove-if-not #'buffer-live-p buffer-list))))))

;;;###autoload
(defun xandeer/kill-buried-buffers (&optional buffer-list interactive)
  "Kill buffers that are buried.
If PROJECT-P (universal argument), only kill buried buffers belonging to the
current project."
  (interactive
   (list (xandeer/buried-buffers
          (if current-prefix-arg (xandeer/project-buffer-list)))
         t))
  (mapc #'kill-buffer buffer-list)
  (xandeer/message-or-count
   interactive "Killed %d buried buffers"
   (- (length buffer-list)
      (length (cl-remove-if-not #'buffer-live-p buffer-list)))))

;;;###autoload
(defun xandeer/kill-project-buffers (project &optional interactive)
  "Kill buffers for the specified PROJECT."
  (interactive
   (list (if-let (open-projects (xandeer/open-projects))
             (completing-read
              "Kill buffers for project: " open-projects
              nil t nil nil
              (if-let* ((project-root (xandeer/project-root))
                        (project-root (abbreviate-file-name project-root))
                        ((member project-root open-projects)))
                  project-root))
           (message "No projects are open!")
           nil)
         t))
  (when project
    (let ((buffer-list (xandeer/project-buffer-list project)))
      (xandeer/kill-buffers-fixup-windows buffer-list)
      (xandeer/message-or-count
       interactive "Killed %d project buffers"
       (- (length buffer-list)
          (length (cl-remove-if-not #'buffer-live-p buffer-list)))))))

(provide 'xandeer-core-buffer)
;;; xandeer-core-buffer.el ends here
