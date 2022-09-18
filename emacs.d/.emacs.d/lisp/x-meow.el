;;; x-meow.el --- x-meow -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun x--meow-setqs ()
  (setq meow-cursor-type-insert '(bar . 3))
  (setq meow-motion-remap-prefix "s-")
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty))

(defun x--meow-define-motion-keys ()
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   ;; '("d" . scroll-up-command)
   ;; '("e" . scroll-down-command)
   ))

(defun x--meow-define-leader-keys ()
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . meow-motion-origin-command)
   '("k" . meow-motion-origin-command)
   ;; high frequency keybindings
   ;; '("e" . "C-x C-e")
   ;; '(")" . "C-)")
   ;; '("}" . "C-}")
   ;; '("." . "M-.")
   ;; '("," . "M-,")
   ;; window management
   ;; '("w" . other-window)
   ;; '("W" . delete-window)
   ;; '("o" . delete-other-windows)
   ;; high frequency commands
   ;; '(";" . comment-dwim)
   ;; '("p" . project-find-file)
   ;; '("d" . dired)
   ;; '("b" . switch-to-buffer)
   ;; '("f" . org-roam-node-find)
   ;; '("i" . imenu)
   ;; '("a" . magit)
   ;; toggles
   '("L" . display-line-numbers-mode)
   ;; '("t" . telega)
   ;; '("r" . org-roam-mode)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)))

;;; find and till
(defun x--meow-search-forward (s n)
  (search-forward-regexp (pinyinlib-build-regexp-string s) nil t n))

(defun x--meow-search-backward (s n)
  (search-backward-regexp (pinyinlib-build-regexp-string s) nil t n))

(defun x/meow--find-continue-forward ()
  (when meow--last-find
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-find)))
      (when (x--meow-search-forward ch-str 1)
        (meow--hack-cursor-pos (point))))))

(defun x/meow--find-continue-backward ()
  (when meow--last-find
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-find)))
      (x--meow-search-backward ch-str 1))))

(defun x/meow-find (n &optional prompt expand)
  "Find the next N char read from minibuffer."
  (interactive "p")
  (let* ((case-fold-search nil)
         (ch (read-char (or prompt (message "Find(%d):" n))))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         end)
    (save-mark-and-excursion
      (setq end (search-forward-regexp (pinyinlib-build-regexp-string ch-str) nil t n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (thread-first
        (meow--make-selection '(select . find)
                              beg end expand)
        (meow--select))
      (setq meow--last-find ch)
      (meow--maybe-highlight-num-positions
       '(x/meow--find-continue-backward . x/meow--find-continue-forward)))))

(defun x/meow-find-expand (n)
  (interactive "p")
  (x/meow-find n (message "Expand find(%d):" n) t))

(defun x/meow--till-continue-forward ()
  (when meow--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-till)))
      (when (< (point) (point-max))
        (forward-char 1)
        (when (x--meow-search-forward ch-str 1)
          (backward-char 1)
          (meow--hack-cursor-pos (point)))))))

(defun x/meow--till-continue-backward ()
  (when meow--last-till
    (let ((case-fold-search nil)
          (ch-str (char-to-string meow--last-till)))
      (when (> (point) (point-min))
        (backward-char 1)
        (when (x--meow-search-backward ch-str 1)
          (forward-char 1)
          (point))))))

(defun x/meow-till (n &optional prompt expand)
  "Forward till the next N char read from minibuffer."
  (interactive "p")
  (let* ((case-fold-search nil)
         (ch (read-char (message (or prompt "Till(%d):") n)))
         (ch-str (if (eq ch 13) "\n" (char-to-string ch)))
         (beg (point))
         (fix-pos (if (< n 0) 1 -1))
         end)
    (save-mark-and-excursion
      (if (> n 0) (forward-char 1) (forward-char -1))
      (setq end (x--meow-search-forward ch-str n)))
    (if (not end)
        (message "char %s not found" ch-str)
      (thread-first
        (meow--make-selection '(select . till)
                              beg (+ end fix-pos) expand)
        (meow--select))
      (setq meow--last-till ch)
      (meow--maybe-highlight-num-positions
       '(x/meow--till-continue-backward . x/meow--till-continue-forward)))))

(defun x/meow-till-expand (n)
  (interactive "p")
  (x/meow-till n (message "Expand till(%d):" n) t))

(defun x--meow-define-normal-keys ()
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-inner-of-thing)
   '("'" . meow-bounds-of-thing)
   '("." . repeat)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change-save)
   '("C" . meow-change-save)
   '("d" . meow-reverse)
   '("D" . x/duplicate-line)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . x/meow-find)
   '("F" . x/meow-find-expand)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-insert-at-begin)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("p" . meow-yank)
   '("P" . meow-yank-pop)
   '("q" . quit-window)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . er/expand-region)
   '("t" . x/meow-till)
   '("T" . x/meow-till-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("&" . meow-query-replace)
   '("%" . anzu-query-replace-regexp)
   '("\\" . quoted-insert)
   '("\/" . meow-visit)
   '("<escape>" . meow-cancel)))

;; (defun x--meow-enable-local-insert ()
;;   "Set insert as default mode."
;;   (setq-default meow-normal-mode nil)
;;   (setq-default meow-insert-mode t))

;; (advice-add 'meow--global-enable :after 'x--meow-enable-local-insert)

(defun x--meow-setup ()
  "Meow setup."
  (x--meow-setqs)
  (x--meow-define-motion-keys)
  (x--meow-define-leader-keys)
  (x--meow-define-normal-keys))

(x/append-init-hook #'meow-global-mode)
(with-eval-after-load 'meow-core
  (require 'meow-cheatsheet)
  (require 'meow-helpers)
  (x--meow-setup))

;;; special modes
(defun x/meow-insert-with-timer (delay)
  (lambda ()
    (run-with-idle-timer delay nil #'meow-insert)))

(add-hook 'eshell-mode-hook (x/meow-insert-with-timer 0.1))
(add-hook 'vterm-mode-hook (x/meow-insert-with-timer 0.1))
(add-hook 'comint-mode-hook (x/meow-insert-with-timer 0.1))
(add-hook 'org-mode-hook (x/meow-insert-with-timer 0.1))
(add-hook 'prog-mode-hook (x/meow-insert-with-timer 0.1))
(add-hook 'text-mode-hook (x/meow-insert-with-timer 0.1))
(add-hook 'telega-chat-mode-hook (x/meow-insert-with-timer 0.1))

(provide 'x-meow)
;;; x-meow.el ends here
