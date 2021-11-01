;;; init-avy.el --- avy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(straight-register-package
 '(pinyinlib :host github
             :repo "xlshiz/pinyinlib.el"))

(leaf pinyinlib
  :straight t
  :require t
  :after ivy-prescient
  :config
  (require 'pinyin-xiaohe)
  (setq pinyinlib--simplified-char-table 'xr/pinyinlib--simplified-xiaohe)
  (defun x/pinyin-regexp-helper (str)
    "Construct pinyin regexp for STR."
    (cond ((equal str "\\).*?\\(") "\\).*?\\(")
          (t (pinyinlib-build-regexp-string str t))))

  (defun x/pinyinlib-build-regexp-string (str)
    "Build a pinyin regexp sequence from STR."
    (cond ((equal str " ") "\\).*?\\(")
          ((equal str "") nil)
          (t str)))

  (defun pinyin-to-utf8 (str)
    "Convert STR to UTF-8."
    (cond ((equal 0 (length str)) nil)
          (t (concat
              "\\("
              (mapconcat
               #'x/pinyinlib-build-regexp-string
               (remove nil (mapcar #'x/pinyin-regexp-helper (split-string str "")))
               "")
              "\\)"))))

  (cl-defun xr/prescient-pinyin-regexp (query &key with-group
                                              &allow-other-keys)
    (prescient-with-group (pinyin-to-utf8 query)
                          (eq with-group 'all)))

  (add-to-list 'prescient-filter-alist '(pinyin . xr/prescient-pinyin-regexp)))

(leaf avy
  :straight t
  :custom
  (avy-timeout-seconds . 0.3)
  (avy-all-windows . 'all-frames)
  (avy-dispatch-alist
   . '(
       ;; (?k . avy-action-kill-stay)
       ;;  (?K . avy-action-kill-whole-line)
       ;;  (?t . avy-action-teleport)
       ;;  (?T . avy-action-teleport-whole-line)
       ;;  (?m . avy-action-mark)
       ;;  (?  . avy-action-mark-to-char)
       ;;  (?w . avy-action-copy)
       ;;  (?W . avy-action-copy-whole-line)
       ;;  (?y . avy-action-yank)
       ;;  (?Y . avy-action-yank-line)
        (?. . avy-action-embark)
        ))
   :bind
   (:isearch-mode-map
   ("M-j" . avy-isearch))
  :bind
  ("M-g a"   . beginning-of-buffer)
  ("M-g e"   . end-of-buffer)
  ("M-g M-a" . beginning-of-buffer-other-window)
  ("M-g M-e" . end-of-buffer-other-window)
  ("M-g v"   . scroll-other-window)
  ("M-g M-v" . scroll-other-window-down)
  ("M-g l"   . avy-goto-line)
  :config
  ;; (setq avy-timeout-seconds 0.3)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-define (pt)
    (save-excursion
      (goto-char pt)
      (sdcv-search-pointer))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-helpful (pt)
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  ;; (setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        ;; (alist-get ?= avy-dispatch-alist) 'avy-action-define
        ;; (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

  (defun xr/pulse-momentarily ()
    (cl-destructuring-bind (beg . end)
        (bounds-of-thing-at-point 'sexp)
      (pulse-momentary-highlight-region beg end 'next-error)))
  (advice-add 'avy-goto-char-timer :after #'xr/pulse-momentarily))

(setq xr/ace-mode nil)
;;;###autoload
(defun xr/ace-mode-p ()
  "Whether ace goto word."
  (bound-and-true-p xr/ace-mode))
(leaf ace-pinyin
  :straight t
  :hook (after-init-hook . ace-pinyin-global-mode)
  :bind*
  ("M-j" . xr/ace-goto-char-timer)
  ("M-k" . xr/ace-pinyin-goto-char-1)
  :config
  (defun xr/ace-goto-char-timer ()
    "Like the `avy-goto-char-timer`."
    (interactive)
    (setq xr/ace-mode t)
    (avy-with xr/ace-goto-char-timer
      (setq avy--old-cands
            (avy--read-candidates
             (lambda (str)
               (pinyinlib-build-regexp-string
                str
                (not ace-pinyin-enable-punctuation-translation)
                (not ace-pinyin-simplified-chinese-only-p)))))
      (avy-process avy--old-cands))
    (setq xr/ace-mode nil))
  (defun xr/ace-pinyin-goto-char-2 ()
    "Ace-pinyin replacement of `avy-goto-char-2'."
    (interactive)
    (setq xr/ace-mode t)
    (call-interactively 'ace-pinyin-jump-char-2)
    (setq xr/ace-mode nil))
  (defun xr/ace-pinyin-goto-word-1 ()
    "Ace-pinyin replacement of `avy-goto-word-1'."
    (interactive)
    (setq xr/ace-mode t)
    (call-interactively 'ace-pinyin-goto-word-1)
    (setq xr/ace-mode nil))
  (defun xr/ace-pinyin-goto-char-1 ()
    "Ace-pinyin replacement of `avy-goto-char'."
    (interactive)
    (setq xr/ace-mode t)
    (call-interactively 'ace-pinyin-jump-char)
    (setq xr/ace-mode nil))
  ;; Another way
  (defun -xr/ace-pinyin-goto-word-1 ()
    "Ace-pinyin replacement of `avy-goto-word-1'."
    (interactive)
    (let ((ace--input-method current-input-method)
          (ace--buffer (buffer-file-name)))
      (when ace--input-method (toggle-input-method))
      (call-interactively 'ace-pinyin-goto-word-1)
      (when (and ace--input-method
                 (string-equal (buffer-file-name) ace--buffer))
        (toggle-input-method)))))

(provide 'init-avy)
;;; init-avy.el ends here
