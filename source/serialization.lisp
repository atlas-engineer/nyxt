(in-package :s-serialization)
;; This file contains a bunch of hot-patches for cl-prevalence's s-exp serialization.

;; TODO: Report upstream.
;; https://github.com/40ants/cl-prevalence/issues/2
(defmethod serialize-sexp-internal ((object pathname) stream serialization-state)
  "Serialize pathname OBJECT to it's printed representation starting with #P.
Note: Function serialization is not part of the original cl-prevalence."
  (declare (ignore serialization-state))
  (prin1 object stream))

(export '*local*)
(defvar *local-package* nil
  "If `*local-package*' designates a package instance, don't print the package
  prefix of its symbols.")

;; TODO: Report upstream.
(defun print-symbol (symbol stream)
  (let ((package (symbol-package symbol))
	(name (prin1-to-string symbol)))
    (cond ((eq package +cl-package+) (write-string "CL:" stream))
	  ((eq package +keyword-package+) (write-char #\: stream))
          ((eq package *local-package*)
           ;; Don't print the package prefix.
           nil)
	  (package (s-xml:print-string-xml (package-name package) stream)
                   (write-string "::" stream))
          (t (write-string "#:" stream)))
    (if (char= (char name (1- (length name))) #\|)
        (write-string name stream :start (position #\| name))
      (write-string name stream :start (1+ (or (position #\: name :from-end t) -1))))))

(export '*one-element-per-line*)
(defvar *one-element-per-line* nil
  "If non-nil, print one element per line in sequence types.")

(defun maybe-newline (stream)
  (when *one-element-per-line*
    (write-char #\newline stream)))

;; TODO: Report upstream.
(defmethod serialize-sexp-internal ((object sequence) stream serialization-state)
  "Like the original `serialize-sexp-internal' but uses `*one-element-per-line* to "
  (flet ((proper-sequence (length)
           (let ((id (set-known-object serialization-state object)))
             (write-string "(:SEQUENCE " stream)
             (prin1 id stream)
             (write-string " :CLASS " stream)
             (print-symbol (etypecase object (list 'list) (vector 'vector)) stream)
             (write-string " :SIZE " stream)
             (prin1 length stream)
             (unless (zerop length)
               (write-string " :ELEMENTS (" stream)
               (maybe-newline stream)
               (map nil
                    #'(lambda (element)
                        (write-string " " stream)
                        (serialize-sexp-internal element stream serialization-state)
                        (maybe-newline stream))
                    object))
             (write-string " ) )" stream)))
         (improper-list ()
           (let ((id (set-known-object serialization-state object)))
             (write-string "(:CONS " stream)
             (prin1 id stream)
             (write-char #\Space stream)
             (serialize-sexp-internal (car object) stream serialization-state)
             (write-char #\Space stream)
             (serialize-sexp-internal (cdr object) stream serialization-state)
             (write-string " ) " stream))))
    (let ((id (known-object-id serialization-state object)))
      (if id
          (progn
            (write-string "(:REF . " stream)
            (prin1 id stream)
            (write-string ")" stream))
          (multiple-value-bind (seq-type length) (sequence-type-and-length object)
            (ecase seq-type
              ((:proper-sequence :proper-list) (proper-sequence length))
              ((:dotted-list :circular-list) (improper-list))))))))
