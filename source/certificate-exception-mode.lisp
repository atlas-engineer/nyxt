;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/certificate-exception-mode
  (:use :common-lisp :nyxt)
  (:documentation "Certificate exception mode"))
(in-package :nyxt/certificate-exception-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-nyxt-package-nicknames))

(sera:export-always '*default-certificate-exceptions*)
(defparameter *default-certificate-exceptions* '()
  "List of hostnames for which to ignore certificate errors.
See the `add-domain-to-certificate-exceptions' command.")

(define-mode certificate-exception-mode ()
  "Enable ignoring of certificate errors.
This can apply to specific buffers.
See the `add-domain-to-certificate-exceptions' command."
  ((certificate-exceptions *default-certificate-exceptions*
                           :type list-of-strings)
   (destructor
    (lambda (mode)
      (setf (certificate-exceptions (buffer mode)) nil)))
   (constructor
    (lambda (mode)
      (setf (certificate-exceptions (buffer mode)) (certificate-exceptions mode))))))

(define-command add-domain-to-certificate-exceptions (&optional (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate exception list.
This is only effective if `certificate-exception-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-exceptions*' in your init file:

\(setf nyxt/certificate-exception-mode:*default-certificate-exceptions*
      '(\"nyxt.atlas.engineer\" \"example.org\"))"
  (if (find-submode buffer 'certificate-exception-mode)
      (let ((input (first (prompt
                           :prompt "URL host to add to exception list:"
                           :input (render-url (url buffer))
                           :sources (list
                                     (make-instance 'prompter:raw-source
                                                    :name "URL")
                                     (make-instance 'nyxt/web-mode::user-history-all-source
                                                         :buffer buffer))))))
        (sera:and-let* ((url (if (stringp input)
                                 (quri:uri input)
                                 (url (htree:data input))))
                        (host (and (not (url-empty-p url))
                                   (quri:uri-host url))))
          (echo "Added exception for ~s." host)
          (pushnew host (certificate-exceptions buffer) :test #'string=)))
      (echo "Enable certificate-exception-mode first.")))

;; TODO: Implement command remove-domain-from-certificate-exceptions.
;;       Currently it is not possible due to WebKit limitations.
