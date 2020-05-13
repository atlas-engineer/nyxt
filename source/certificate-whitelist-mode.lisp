(uiop:define-package :next/certificate-whitelist-mode
  (:use :common-lisp :trivia :next)
  (:documentation "Certificate whitelist mode"))
(in-package :next/certificate-whitelist-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(sera:export-always '*default-certificate-whitelist*)
(defparameter *default-certificate-whitelist* '()
  "List of hostnames for which to ignore certificate errors.
See the `add-domain-to-certificate-whitelist' command.")

(define-mode certificate-whitelist-mode ()
  "Enable ignoring of certificate errors.
This can apply to specific buffers.
See the `add-domain-to-certificate-whitelist' command."
  ((certificate-whitelist :initarg :certificate-whitelist
                          :accessor certificate-whitelist
                          :type list-of-strings
                          :initform *default-certificate-whitelist*)
   (destructor
    :initform
    (lambda (mode)
      (setf (certificate-whitelist (buffer mode)) nil)))
   (constructor
    :initform
    (lambda (mode)
      (setf (certificate-whitelist (buffer mode)) (certificate-whitelist mode))))))

(define-command add-domain-to-certificate-whitelist (&optional (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate whitelist.
This is only effective if `certificate-whitelist-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-whitelist*' in your init file:

\(setf next/certificate-whitelist-mode:*default-certificate-whitelist*
      '(\"next.atlas.engineer\" \"example.org\"))"
  (if (find-submode buffer 'certificate-whitelist-mode)
      (if (url buffer)
          (let ((host (host (url buffer))))
            (log:info host)
            (pushnew host (certificate-whitelist buffer) :test #'string=))
          (echo "Buffer has no URL."))
      (echo "Enable certificate-whitelist-mode first.")))

;; TODO: Implement command remove-domain-from-certificate-whitelist.
;;       Currently it is not possible due to WebKit limitations.
