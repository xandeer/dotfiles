;;; xandeer-tools-company-function.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xandeer

;;; Commentary:

;; Xandeer's Emacs Configuration Company Mode.

;;; Code:

(require 'company)
;; Borrow from doom emacs.

;;;###autoload
(defvar xandeer/company-backend-alist
  '((text-mode company-dabbrev company-yasnippet company-ispell company-files)
    (prog-mode company-capf company-yasnippet company-files)
    (conf-mode company-capf company-dabbrev-code company-yasnippet company-files))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")


;;;###autoload
(defun xandeer/add-company-backend (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.
Examples:
 (xandeer/add-company-backend 'js2-mode
   'company-tide 'company-yasnippet)
 (xandeer/add-company-backend 'sh-mode
   '(company-shell :with company-yasnippet))
 (xandeer/add-company-backend '(c-mode c++-mode)
   '(:separate company-irony-c-headers company-irony))
 (xandeer/add-company-backend 'sh-mode nil)  ; unsets backends for sh-mode"

  (declare (indent defun))
  (dolist (mode (xandeer-enlist modes))
    (if (null (car backends))
        (setq xandeer/company-backend-alist
              (delq (assq mode xandeer/company-backend-alist)
                    xandeer/company-backend-alist))
      (setf (alist-get mode xandeer/company-backend-alist)
            backends))))


;;;###autoload
(defun xandeer/company-backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode xandeer/company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in xandeer/company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                              (and (boundp mode)
                                 (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))


;;;###autoload
(defun xandeer/company-init-backends-h ()
  "Set `company-backends' for the current buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
     buffer-read-only
     (xandeer/temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
     (setq-local company-backends (xandeer/company-backends))))


(put 'xandeer/company-init-backends-h 'permanent-local-hook t)


;;;###autoload
(defun xandeer/company-has-completion-p ()
  "Return non-nil if a completion candidate exists at point."
  (and (company-manual-begin)
       (= company-candidates-length 1)))


;;;###autoload
(defun xandeer/company-toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'company)
  (setq company-idle-delay (unless company-idle-delay 0.2))
  (message "Auto completion %s"
           (if company-idle-delay "enabled" "disabled")))


;;;###autoload
(defun xandeer/company-complete ()
  "Bring up the completion popup. If only one result, complete it."
  (interactive)
  (require 'company)
  (when (ignore-errors
          (/= (point)
              (cdr (bounds-of-thing-at-point 'symbol))))
    (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))


;;;###autoload
(defun xandeer/company-dabbrev ()
  "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev'
    everywhere else."
  (interactive)
  (call-interactively
   (if (derived-mode-p 'prog-mode)
       #'company-dabbrev-code
     #'company-dabbrev)))


;;;###autoload
(defun xandeer/company-whole-lines (command &optional arg &rest ignored)
  "`company-mode' completion backend that completes whole-lines, akin to vim's
    C-x C-l."
  (interactive (list 'interactive))
  (require 'company)
  (pcase command
    (`interactive (company-begin-backend 'xandeer/company-whole-lines))
    (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
    (`candidates
     (all-completions
      arg
      (delete-dups
       (split-string
        (replace-regexp-in-string
         "^[\t\s]+" ""
         (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
                 (buffer-substring-no-properties (line-end-position) (point-max))))
        "\\(\r\n\\|[\n\r]\\)" t))))))


;;;###autoload
(defun xandeer/company-dict-or-keywords ()
  "`company-mode' completion combining `company-dict' and `company-keywords'."
  (interactive)
  (require 'company-dict)
  (require 'company-keywords)
  (let ((company-backends '((company-keywords company-dict))))
    (call-interactively #'company-complete)))


;;;###autoload
(defun xandeer/company-dabbrev-code-previous ()
  "TODO"
  (interactive)
  (require 'company-dabbrev)
  (let ((company-selection-wrap-around t))
    (call-interactively #'xandeer/company-dabbrev)
    (company-select-previous-or-abort)))

(provide 'xandeer-tools-company-function)
;;; xandeer-tools-company-function.el ends here
