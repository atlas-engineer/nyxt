;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *inspected-values* (tg:make-weak-hash-table :test 'equal))

(export-always 'sequence-p)
(defun sequence-p (object)
  "Return true if OBJECT is a sequence that's not a string."
  (typep object '(and sequence (not string))))

(export-always 'scalar-p)
(defun scalar-p (object)
  ;; REVIEW: List direct T subclasses instead?
  "Return true if OBJECT is of one of the following types:
- symbol,
- character,
- string,
- non-complex number."
  (funcall (alex:disjoin
            'symbolp
            'characterp
            'stringp
            (alex:rcurry 'typep '(and number (not complex))))
           object))

(export-always 'inspected-value)
(defmethod inspected-value (id)
  (gethash id *inspected-values*))

(defmethod (setf inspected-value) (new-value id)
  (setf (gethash id *inspected-values*) new-value))

(defun escaped-literal-print (value)
  (spinneret:with-html-string
    (:code (:raw (spinneret::escape-string (prin1-to-string value))))))

(defun link-to (object)
  (let ((id (get-unique-identifier *browser*)))
    (if (scalar-p object)
        (spinneret:with-html-string
          (:raw (escaped-literal-print object)))
        (progn
          (setf (inspected-value id) object)
          (spinneret:with-html-string
            (:a :href (nyxt-url 'describe-value :id id)
                (:raw (escaped-literal-print object))))))))

(export-always 'value->html)
(defgeneric value->html (value &optional compact-p)
  (:method :around (value &optional compact-p)
    (let ((spinneret:*html-style* :tree)
          (*print-case* :downcase))
      (call-next-method value compact-p)))
  (:method (value &optional compact-p)
    (declare (ignore compact-p))
    (escaped-literal-print value))
  (:method ((value null) &optional compact-p)
    (declare (ignore compact-p))
    (escaped-literal-print value))
  (:method ((value string) &optional compact-p)
    (declare (ignore compact-p))
    (escaped-literal-print value))
  (:documentation "Produce HTML showing the structure of the VALUE.
If it's COMPACT-P, compress the output.

Specialize this generic function if you want to have a different markup for Lisp
values in help buffers, REPL and elsewhere."))

(defmethod value->html ((value function) &optional compact-p)
  (spinneret:with-html-string
    (let ((name (first (alex:ensure-list (swank-backend:function-name value)))))
      (cond
        ((and name (eq name 'lambda) compact-p)
         (:raw (link-to value)))
        ((and name (eq name 'lambda))
         (multiple-value-bind (expression closure-p name)
             (function-lambda-expression value)
             (:dl
              (:dt "name")
              (:dd (:raw (escaped-literal-print name)))
              (:dt "code")
              (:dd (:raw (escaped-literal-print expression)))
              (:dt "closure-p")
              (:dd (:raw (value->html closure-p))))))
        (name
         (:a :href (nyxt-url 'describe-function :function name)
             (:raw (escaped-literal-print value))))
        (t (:raw (escaped-literal-print value)))))))

(defmethod value->html ((value list) &optional compact-p)
  (spinneret:with-html-string
    (:div
     :style "overflow-x: auto"
     (cond
       ((trivial-types:property-list-p value)
        (:table
         (unless compact-p
           (:caption "Property list"))
         (:thead (loop for key in value by #'cddr
                       collect (:th (:raw (escaped-literal-print key)))))
         (:tbody
          (:tr
           (loop for val in (rest value) by #'cddr
                 collect (:td (:raw (value->html val t))))))))
       ((trivial-types:association-list-p value)
        (:table
         (unless compact-p
           (:caption "Property list")
           (:thead (:th "Property") (:th "Value")))
         (:thead
          (dolist (e value)
            (:th (:raw (escaped-literal-print (car e))))))
         (:tbody
          (:tr
           (dolist (e value)
             (:td (:raw (value->html (cdr e) t))))))))
       ((and (trivial-types:proper-list-p value)
             (not (alexandria:circular-list-p value))
             (not (alexandria:circular-tree-p value)))
        (:ul
         (dotimes (i (length value))
           (:li (:raw (value->html (elt value i) t))))))
       (t (:raw (escaped-literal-print value)))))))

(defmethod value->html ((value array) &optional compact-p)
  (spinneret:with-html-string
    (if (uiop:emptyp value)
        (:raw (call-next-method))
        (:div
         :style "overflow-x: auto"
         (case (length (array-dimensions value))
           (1 (:table
               (unless compact-p
                 (:caption "Array")
                 (:thead
                  (:th :colspan (alex:lastcar (array-dimensions value)) "Elements")))
               (:tbody
                (:tr
                 (loop for e across value
                       collect (:td (:raw (value->html e t))))))))
           (2 (:table
               (unless compact-p
                 (:caption "Array")
                 (:thead
                  (:th :colspan (alex:lastcar (array-dimensions value)) "Elements")))
               (:tbody
                (loop with height = (array-dimension value 0)
                      and width = (array-dimension value 1)
                      for y below height
                      collect (:tr (loop for x below width
                                         collect (:td (:raw (value->html (aref value y x) t)))))))))
           (otherwise (:raw (call-next-method))))))))

(defmethod value->html ((value sequence) &optional compact-p)
  (declare (ignore compact-p))
  (spinneret:with-html-string
    (if (uiop:emptyp value)
        (:raw (escaped-literal-print value))
        (:ul
         (dotimes (i (length value))
           (:li (:raw (value->html (elt value i) t))))))))

(defmethod value->html ((value hash-table) &optional compact-p)
  (spinneret:with-html-string
    (:div
     :style "overflow-x: auto"
     (alex:if-let ((keys (alex:hash-table-keys value)))
       (:table
        (unless compact-p
          (:caption "Hash-table"))
        (:thead (dolist (key keys)
                  (:th (:raw (escaped-literal-print key)))))
        (:tbody
         (:tr
          (dolist (key keys)
            (:td (:raw (value->html (gethash key value) t)))))))
       (:raw (call-next-method))))))

(defun print-complex-object (value compact-p)
  (if compact-p
      (link-to value)
      (spinneret:with-html-string
        (alex:if-let ((slot-names (mapcar #'closer-mop:slot-definition-name
                                          (closer-mop:class-slots (class-of value)))))
          (:dl
           (dolist (slot-name slot-names)
             (:dt (prin1-to-string slot-name))
             (:dd (:raw (value->html (slot-value value slot-name) t)))))
          (:raw (escaped-literal-print value))))))

(defmethod value->html ((value standard-object) &optional compact-p)
  (print-complex-object value compact-p))

(defmethod value->html ((value structure-object) &optional compact-p)
  (print-complex-object value compact-p))
