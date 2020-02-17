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
            (whitelist (certificate-whitelist mode)))))))

(define-command add-domain-to-certificate-whitelist (&optional (buffer (current-buffer)))
  "Add the current domain to the buffer's certificate whitelist."
  (if (url buffer)
      (let ((domain (quri:uri-host (quri:uri (url buffer))))
            (certificate-whitelist (certificate-whitelist buffer)))
        (pushnew domain (whitelist certificate-whitelist) :test #'string=)
        (setf (certificate-whitelist buffer) certificate-whitelist))
      (echo "Buffer has no URL.")))

;; TODO: Implement command remove-domain-from-certificate-whitelist.
;;       Currently it is not possible due to WebKit limitations.
