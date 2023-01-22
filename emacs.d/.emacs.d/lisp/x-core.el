;;; x-core.el --- Unified Concepts with other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'first #'car)
(defalias 'rest #'cdr)

(defun zero? (n)
  "Whether `N' equals `0'?"
  (= n 0))

(defun empty? (lst)
  "Whether `LST' is empty?"
  (zero? (length lst)))

(provide 'x-core)
;;; x-core.el ends here
