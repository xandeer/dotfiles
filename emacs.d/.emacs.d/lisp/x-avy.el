;;; x-avy.el --- avy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(defun x/pulse-momentarily ()
  (cl-destructuring-bind (beg . end)
      (bounds-of-thing-at-point 'sexp)
    (pulse-momentary-highlight-region beg end 'next-error)))
(advice-add 'avy-goto-char-timer :after #'x/pulse-momentarily)

(setq avy-timeout-seconds 0.3)
(setq avy-all-windows 'all-frames)
(setq avy-single-candidate-jump t)
(setq avy-dispatch-alist
      '(
        ;; (?k . avy-action-kill-stay)
        ;;  (?K . avy-action-kill-whole-line)
        ;;  (?t . avy-action-teleport)
        ;;  (?T . avy-action-teleport-whole-line)
        (?m . avy-action-mark)
        ;;  (?  . avy-action-mark-to-char)
        ;;  (?w . avy-action-copy)
        ;;  (?W . avy-action-copy-whole-line)
        ;;  (?y . avy-action-yank)
        ;;  (?Y . avy-action-yank-line)
        (?. . avy-action-embark)))

(define-key isearch-mode-map (kbd "M-j") 'avy-isearch)

(provide 'x-avy)
;;; x-avy.el ends here
