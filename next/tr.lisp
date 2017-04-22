(defpackage :eql-tr
  (:use :common-lisp :eql))

(in-package :eql-tr)

(defparameter *sources* (make-hash-table :test 'equal))

(progn
  (when (probe-file "tr.h")
    (delete-file "tr.h"))
  (define-compiler-macro tr (&whole form src &optional con (n -1))
    (let* ((source (ignore-errors (eval src)))
           (context* (ignore-errors (eval con)))
           (context (if (stringp context*)
                        context*
                        (file-namestring *compile-file-truename*))))
      (with-open-file (out "tr.h" :direction :output :if-exists :append :if-does-not-exist :create)
        (if (stringp source)
            (unless (gethash (cons source context) *sources*)
              (setf (gethash (cons source context) *sources*) t)
              (format out "QCoreApplication::translate(~S, ~S, 0~A);~%"
                      context
                      source
                      (if (= -1 n) "" (format nil ", ~D" n))))
            (error (format nil "[TR: error] ~S from context ~S doesn't evaluate to a string" src context)))))
    form))
