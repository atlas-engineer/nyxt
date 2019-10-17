;;; conditions.lisp --- conditions used in Next

(in-package :next)

(export '(next-condition))

(define-condition next-condition (error)
  ((message :initarg :message :accessor next-condition-message))
  (:report (lambda (c stream)
             (format stream "~a" (slot-value c 'message))))
  (:documentation "An error internal to Next. It should abort the ongoing command, but not the whole process."))
