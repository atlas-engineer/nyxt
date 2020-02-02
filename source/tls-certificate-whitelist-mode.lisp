(uiop:define-package :next/tls-certificate-whitelist-mode
    (:use :common-lisp :trivia :next)
  (:documentation "TLS certificate whitelist mode"))
(in-package :next/tls-certificate-whitelist-mode)
(annot:enable-annot-syntax)

@export
(defparameter *default-tls-certificate-whitelist*
  (make-instance next::*tls-certificate-whitelist-class*
                 :whitelist '()))

(define-mode tls-certificate-whitelist-mode ()
  "Enable ignoring of TLS certificate errors.
This can apply to specific buffer."
  ((tls-certificate-whitelist :initarg :tls-certificate-whitelist
          :accessor tls-certificate-whitelist
          :type tls-certificate-whitelist
          :initform *default-tls-certificate-whitelist*)
   (destructor
    :initform
    (lambda (mode)
      (setf (tls-certificate-whitelist (buffer mode)) nil)))
   (constructor
    :initform
    (lambda (mode)
      (setf (tls-certificate-whitelist (buffer mode)) (tls-certificate-whitelist mode))
      (echo "TLS certificate host whitelist set to ~a."
            (whitelist (tls-certificate-whitelist mode)))))))
