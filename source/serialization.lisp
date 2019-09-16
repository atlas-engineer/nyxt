(in-package :s-serialization)

;; TODO: Test named functions + lambdas.
;; TODO: Report upstream if good enough.
(defmethod serialize-sexp-internal ((object function) stream serialization-state)
  "Serialize function OBJECT.
If the function is named FOO, serialized is to \"#'FOO\".
Note: Function serialization is not part of the original cl-prevalence."
  (declare (ignore serialization-state))
  (let ((function-symbol (nth-value 2 (function-lambda-expression object))))
    (unless (symbolp function-symbol)
      ;; (error "Cannot serialize anonymous functions (lambdas)")
      (setf function-symbol nil))
    (write-string "(:FUNCTION . " stream)
    (print-symbol function-symbol stream)
    (write-string ")" stream)))

;; TODO: Report upstream.
(defmethod serialize-sexp-internal ((object pathname) stream serialization-state)
  "Serialize pathname OBJECT to it's printed representation starting with #P.
Note: Function serialization is not part of the original cl-prevalence."
  (declare (ignore serialization-state))
  (prin1 object stream))

;; The following adds function support to deserialization:
(defun deserialize-sexp-internal (sexp deserialized-objects)
  (if (atom sexp)
      sexp
      (ecase (first sexp)
        (:sequence (destructuring-bind (id &key class size elements) (rest sexp)
                     (let ((sequence (make-sequence class size)))
                       (setf (gethash id deserialized-objects) sequence)
                       (map-into sequence
                                 #'(lambda (x) (deserialize-sexp-internal x deserialized-objects))
                                 elements))))
        (:hash-table (destructuring-bind (id &key test size rehash-size rehash-threshold entries) (rest sexp)
                       (let ((hash-table (make-hash-table :size size
                                                          :test test
                                                          :rehash-size rehash-size
                                                          :rehash-threshold rehash-threshold)))
                         (setf (gethash id deserialized-objects) hash-table)
                         (dolist (entry entries)
                           (setf (gethash (deserialize-sexp-internal (first entry) deserialized-objects) hash-table)
                                 (deserialize-sexp-internal (rest entry) deserialized-objects)))
                         hash-table)))
        (:object (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (make-instance class)))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot))
                               (deserialize-sexp-internal (rest slot) deserialized-objects))))
                     object)))
        (:struct (destructuring-bind (id &key class slots) (rest sexp)
                   (let ((object (funcall (intern (concatenate 'string "MAKE-" (symbol-name class))
                                                  (symbol-package class)))))
                     (setf (gethash id deserialized-objects) object)
                     (dolist (slot slots)
                       (when (slot-exists-p object (first slot))
                         (setf (slot-value object (first slot))
                               (deserialize-sexp-internal (rest slot) deserialized-objects))))
                     object)))
        (:cons (destructuring-bind (id cons-car cons-cdr) (rest sexp)
                 (let ((conspair (cons nil nil)))
                   (setf (gethash id deserialized-objects)
                         conspair)
                   (rplaca conspair (deserialize-sexp-internal cons-car deserialized-objects))
                   (rplacd conspair (deserialize-sexp-internal cons-cdr deserialized-objects)))))
        (:ref (gethash (rest sexp) deserialized-objects))
        (:function (symbol-function (rest sexp))))))
