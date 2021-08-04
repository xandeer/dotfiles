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
  :commands pinyinlib-build-regexp-string
  :init
  (setq pinyinlib--simplified-char-table 'pinyinlib--simplified-xiaohe)
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

  (defun prescient-filter-regexps (query &optional with-groups)
    "Convert QUERY to list of regexps.
Each regexp must match the candidate in order for a candidate to
match the QUERY.

If WITH-GROUPS is non-nil, enclose the initials in initialisms
with capture groups. If it is the symbol `all', additionally
enclose literal substrings with capture groups."
    (mapcar
     (lambda (subquery)
       (string-join
        (cl-remove
         nil
         (mapcar
          (lambda (method)
            (pcase method
              (`literal
               (prescient--with-group
                (char-fold-to-regexp subquery)
                (eq with-groups 'all)))
              (`initialism
               (prescient--initials-regexp subquery with-groups))
              (`regexp
               (ignore-errors
                 ;; Ignore regexp if it's malformed.
                 (string-match-p subquery "")
                 subquery))
              (`fuzzy
               (prescient--fuzzy-regexp subquery with-groups))
              (`prefix
               (prescient--prefix-regexp subquery with-groups))
              (`pinyin
               (pinyin-to-utf8 subquery))))
          (pcase prescient-filter-method
            ;; We support `literal+initialism' for backwards
            ;; compatibility.
            (`literal+initialism '(literal initialism))
            ((and (pred listp) x) x)
            (x (list x))))
         :test #'eq)
        "\\|"))
     (prescient-split-query query))))

(leaf avy
  :straight t
  :bind
  ("M-g a"   . beginning-of-buffer)
  ("M-g e"   . end-of-buffer)
  ("M-g M-a" . beginning-of-buffer-other-window)
  ("M-g M-e" . end-of-buffer-other-window)
  ("M-g v"   . scroll-other-window)
  ("M-g M-v" . scroll-other-window-down)
  ("M-g l"   . avy-goto-line))

(setq xr/ace-mode nil)
;;;###autoload
(defun xr/ace-mode-p ()
  "Whether ace goto word."
  (bound-and-true-p xr/ace-mode))
(leaf ace-pinyin
  :straight t
  :hook (after-init-hook . ace-pinyin-global-mode)
  :bind*
  ("M-j" . xr/ace-pinyin-goto-word-1)
  :config
  (defun xr/ace-pinyin-goto-word-1 ()
    "Ace-pinyin replacement of `avy-goto-word-1'."
    (interactive)
    (setq xr/ace-mode t)
    (call-interactively 'ace-pinyin-goto-word-1)
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
