(uiop:define-package :next/certificate-whitelist-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Certificate whitelist mode"))
(in-package :next/certificate-whitelist-mode)
(annot:enable-annot-syntax)

@export
(defparameter *default-certificate-whitelist*
  (make-instance next::*certificate-whitelist-class*
                 :whitelist '()))

(define-mode certificate-whitelist-mode ()
  "Enable ignoring of certificate errors.
This can apply to specific buffers."
  ((certificate-whitelist :initarg :certificate-whitelist
                          :accessor certificate-whitelist
                          :type certificate-whitelist
                          :initform *default-certificate-whitelist*)
   (destructor
    :initform
    (lambda (mode)
      (setf (certificate-whitelist (buffer mode)) nil)))
   (constructor
    :initform
    (lambda (mode)
      (setf (certificate-whitelist (buffer mode)) (certificate-whitelist mode))
      (echo "Certificate host whitelist set to ~a."
            (whitelist (certificate-whitelist mode))))))
  )
