;;; x-telega-cus-edit-patch.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun custom-variable-type (symbol)
  "Return a widget suitable for editing the value of SYMBOL.
If SYMBOL has a `custom-type' property, use that.
Otherwise, try matching SYMBOL against `custom-guess-name-alist' and
try matching its doc string against `custom-guess-doc-alist'."
  (let* ((type (or (get symbol 'custom-type)
		   (and (not (get symbol 'standard-value))
			(custom-guess-type symbol))
		   'sexp))
	 (options (get symbol 'custom-options))
	 (tmp (if (listp type)
		  (copy-sequence type)
		(list type))))
    (when options
      ;; This used to use widget-put, but with strict plists that
      ;; fails when type is an even-length list, eg (repeat character).
      ;; Passing our result through widget-convert makes it a valid widget.
      (setcdr tmp (append (list :options options) (cdr tmp))))
    tmp))

(defun custom-face-get-current-spec-unfiltered (face)
  "Return the current spec for face FACE, without filtering it."
  (let ((spec (or (get face 'customized-face)
		              (get face 'saved-face)
		              (get face 'face-defface-spec)
		              ;; Attempt to construct it.
		              `((t ,(custom-face-attributes-get
			                   face (selected-frame)))))))
    ;; If the user has changed this face in some other way,
    ;; edit it as the user has specified it.
    (if (not (face-spec-match-p face spec (selected-frame)))
        (setq spec `((t ,(face-attr-construct face)))))
    spec))

(defun custom-face-get-current-spec (face)
  "Return the current spec for face FACE, filtering it."
  (custom-pre-filter-face-spec (custom-face-get-current-spec-unfiltered face)))

(defun custom-pre-filter-face-spec (spec)
  "Return SPEC changed as necessary for editing by the face customization widget.
SPEC must be a full face spec."
  (custom-filter-face-spec spec 2))

(defun custom-filter-face-spec (spec filter-index &optional default-filter)
  "Return a canonicalized version of SPEC.
FILTER-INDEX is the index in the entry for each attribute in
`custom-face-attributes' at which the appropriate filter function can be
found, and DEFAULT-FILTER is the filter to apply for attributes that
don't specify one."
  (mapcar (lambda (entry)
	    ;; Filter a single face-spec entry
	    (let ((tests (car entry))
		  (unfiltered-attrs
		   ;; Handle both old- and new-style attribute syntax
		   (if (listp (car (cdr entry)))
		       (car (cdr entry))
		     (cdr entry)))
		  (filtered-attrs nil))
	      ;; Filter each face attribute
	      (while unfiltered-attrs
		(let* ((attr (pop unfiltered-attrs))
		       (pre-filtered-value (pop unfiltered-attrs))
		       (filter
			(or (nth filter-index (assq attr custom-face-attributes))
			    default-filter))
		       (filtered-value
			(if filter
			    (funcall filter pre-filtered-value)
			  pre-filtered-value)))
		  (push filtered-value filtered-attrs)
		  (push attr filtered-attrs)))
	      ;;
	      (list tests filtered-attrs)))
	  spec))

(provide 'x-telega-cus-edit-patch)
;;; x-telega-cus-edit-patch.el ends here
