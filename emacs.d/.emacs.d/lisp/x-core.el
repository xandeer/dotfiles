;;; x-core.el --- Unified Concepts with other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'short-lambda)

;;; Core core
(defalias 'first #'car)
(defalias 'rest #'cdr)
(defalias 'delete-duplicates #'cl-delete-duplicates)
(defalias 'fn #'short-lambda)

;;; Boolean
(defun nil? (o)
  "Whether `O' is `NIL'?"
  (eq o nil))

(defalias 'false? #'nil?)

(defun true? (q)
  "Whether `Q' is not `NIL'?"
  (not (false? q)))

;;; Number
(ert-deftest x/core-zero? ()
  (should (zero? 0))
  (should-not (zero? 1)))
(defun zero? (n)
  "Whether `N' equals `0'?"
  (= n 0))

(defalias 'inc #'1+)
(defalias 'dec #'1-)

(defun pos? (n)
  "Whether `N' is a positive number?"
  (> n 0))

(defun neg? (n)
  "Whether `N' is a negative number?"
  (< n 0))

;;; List
(ert-deftest x/core-empty? ()
  (should (empty? '()))
  (should-not (empty? '(a b c)))
  (should-error (empty? 'a)))
(defun empty? (lst)
  "Whether `LST' is empty?"
  (zero? (length lst)))

(ert-deftest x/core-in? ()
  (should (in? 'a '(a)))
  (should-not (in? 'a '())))
(defalias 'in? #'member)

;; (ert "x/core")

(provide 'x-core)
;;; x-core.el ends here
