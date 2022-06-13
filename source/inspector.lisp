;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *inspected-values* (tg:make-weak-hash-table :test 'equal :weakness :value))

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

(defun ensure-inspected-id (value)
  (maphash
   (lambda (id object)
     (when (equal value object)
       (return-from ensure-inspected-id id)))
   *inspected-values*)
  ;; We cannot use `new-id' directly because it would force the call to
  ;; `inspected-value' to be within the scope of `ensure-inspected-id'.
  (sera:lret ((id (intern (symbol-name (new-id)))))
    (setf (inspected-value id) value)))


(export-always '*inspector-print-length*)
(defvar *inspector-print-length* 50
  "The size of the structure after which to collapse this structure into a link.

Can cause a renderer to choke when set to a high value. Use with caution!")

(defun escaped-literal-print (value)
  (spinneret:with-html-string
    (:code (:raw (spinneret::escape-string
                  (let ((*print-lines* 3))
                    (prin1-to-string value)))))))

(defun link-to (object)
  (if (scalar-p object)
      (spinneret:with-html-string
        (:raw (escaped-literal-print object)))
      (spinneret:with-html-string
        (:a :href (nyxt-url 'describe-value :id (ensure-inspected-id object))
            (:raw (escaped-literal-print object))))))

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
         (:a :href (nyxt-url 'describe-function :fn name)
             (:raw (escaped-literal-print value))))
        (t (:raw (escaped-literal-print value)))))))

(defmethod value->html ((value list) &optional compact-p)
  (spinneret:with-html-string
    (:div
     :style "overflow-x: auto"
     (cond
       ((and compact-p (> (length value) *inspector-print-length*))
        (:raw (link-to value)))
       ((trivial-types:association-list-p value)
        (:table
         (unless compact-p
           (:caption "Association list"))
         (:thead
          (dolist (e value)
            (:th (:raw (value->html (car e) t)))))
         (:tbody
          (:tr
           (dolist (e value)
             (:td (:raw (value->html (cdr e) t))))))))
       ((and (trivial-types:property-list-p value)
             ;; Stricter understanding of property lists:
             ;; -- Even length.
             ;; -- Keys are strictly keywords.
             ;; -- At least one value should be a non-keyword.
             (evenp (length value))
             (loop with all-values-keywords? = t
                   for (key val) on value by #'cddr
                   unless (keywordp key)
                     do (return nil)
                   unless (keywordp val)
                     do (setf all-values-keywords? nil)
                   finally (return (not all-values-keywords?))))
        (:table
         (unless compact-p
           (:caption "Property list"))
         (:thead (loop for key in value by #'cddr
                       collect (:th (:raw (escaped-literal-print key)))))
         (:tbody
          (:tr
           (loop for val in (rest value) by #'cddr
                 collect (:td (:raw (value->html val t))))))))
       ((and (trivial-types:proper-list-p value)
             (not (alexandria:circular-list-p value))
             (not (alexandria:circular-tree-p value)))
        (:ul
         (dotimes (i (length value))
           (:li (:raw (value->html (elt value i) t))))))
       (t (:raw (escaped-literal-print value)))))))

(defmethod value->html ((value array) &optional compact-p)
  (spinneret:with-html-string
    (cond
      ((uiop:emptyp value)
       (:raw (call-next-method)))
      ((and compact-p (> (length value) *inspector-print-length*))
       (:raw (link-to value)))
      (t (:div
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
            (otherwise (:raw (call-next-method)))))))))

(defmethod value->html ((value sequence) &optional compact-p)
  (spinneret:with-html-string
    (cond
      ((uiop:emptyp value)
       (:raw (escaped-literal-print value)))
      ((and compact-p (> (length value) *inspector-print-length*))
       (:raw (link-to value)))
      (t (:ul
          (dotimes (i (length value))
            (:li (:raw (value->html (elt value i) t)))))))))

(defmethod value->html ((value hash-table) &optional compact-p)
  (spinneret:with-html-string
    (:div
     :style "overflow-x: auto"
     (let ((keys (alex:hash-table-keys value)))
       (cond
         ((uiop:emptyp keys)
          (:raw (call-next-method)))
         ((and compact-p (> (hash-table-count value) *inspector-print-length*))
          (:raw (link-to value)))
         (t (:table
             (unless compact-p
               (:caption "Hash-table"))
             (:thead (dolist (key keys)
                       (:th (:raw (escaped-literal-print key)))))
             (:tbody
              (:tr
               (dolist (key keys)
                 (:td (:raw (value->html (gethash key value) t)))))))))))))

(defmethod value->html ((value pathname) &optional compact-p)
  (let* ((namestring (uiop:native-namestring value))
         (mime (mimes:mime namestring)))
    (spinneret:with-html-string
      (:a :href (quri.uri.file:make-uri-file :path namestring)
          namestring
          (:br)
          (cond
            ((and (str:starts-with-p "image/" mime)
                  (not compact-p))
             (:img :src (quri.uri.file:make-uri-file :path namestring)
                   :alt namestring))
            ((and (str:starts-with-p "audio/" mime)
                  (not compact-p))
             (:audio :src (quri.uri.file:make-uri-file :path namestring)
                     :controls t))
            ((and (str:starts-with-p "video/" mime)
                  (not compact-p))
             (:video :src (quri.uri.file:make-uri-file :path namestring)
                     :controls t))
            (t ""))))))

(defun print-complex-object (value compact-p)
  (if compact-p
      (link-to value)
      (spinneret:with-html-string
        (alex:if-let ((slot-names (mapcar #'closer-mop:slot-definition-name
                                          (closer-mop:class-slots (class-of value)))))
          (:dl
           (dolist (slot-name slot-names)
             (:dt (prin1-to-string slot-name))
             (:dd (:raw (value->html (slot-value value slot-name) t))
                  (:button
                   :class "button"
                   :onclick (ps:ps (nyxt/ps:lisp-eval
                                    (:title "change value")
                                    (handler-case
                                        (setf (slot-value value slot-name)
                                              (first
                                               (evaluate
                                                (prompt1
                                                  :prompt (format nil "Set ~a to" slot-name)
                                                  :sources (make-instance 'prompter:raw-source)))))
                                      (nyxt-prompt-buffer-canceled nil))))
                   "change "))))
          (:raw (escaped-literal-print value))))))

(defmethod value->html ((value standard-object) &optional compact-p)
  (print-complex-object value compact-p))

(defmethod value->html ((value structure-object) &optional compact-p)
  (print-complex-object value compact-p))
