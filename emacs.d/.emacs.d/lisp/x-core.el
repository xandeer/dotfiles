;;; x-core.el --- Unified Concepts with other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'first #'car)
(defalias 'rest #'cdr)

(defun nil? (o)
  "Whether `O' is `NIL'?"
  (eq o nil))

(defalias 'false? #'nil?)

(defun true? (q)
  "Whether `Q' is not `NIL'?"
  (not (nil? q)))

(ert-deftest x/core-test-zero? ()
  (should (zero? 0))
  (should-not (zero? 1)))
(defun zero? (n)
  "Whether `N' equals `0'?"
  (= n 0))

(ert-deftest x/core-test-empty? ()
  (should (empty? '()))
  (should-not (empty? '(a b c)))
  (should-error (empty? 'a)))
(defun empty? (lst)
  "Whether `LST' is empty?"
  (zero? (length lst)))

(ert-deftest x/core-test-in? ()
  (should (in? 'a '(a)))
  (should-not (in? 'a '())))
(defalias 'in? #'member)

;; (ert "x/core")

(provide 'x-core)
;;; x-core.el ends here
