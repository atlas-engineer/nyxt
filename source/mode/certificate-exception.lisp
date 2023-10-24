;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/certificate-exception
  (:documentation "Control which invalid certificates to accept or reject.
Important parts are:
- `*default-certificate-exceptions*' variable.
- and `add-domain-to-certificate-exceptions' command."))
(in-package :nyxt/mode/certificate-exception)

(export-always '*default-certificate-exceptions*)
(defparameter *default-certificate-exceptions* '()
  "List of hostnames for which to ignore certificate errors.
See the `add-domain-to-certificate-exceptions' command.")

(define-mode certificate-exception-mode ()
  "Control which invalid certificates to accept or reject.
This applies to specific buffers.
See the `add-domain-to-certificate-exceptions' command."
  ((visible-in-status-p nil)
   (certificate-exceptions *default-certificate-exceptions*
                           :type (list-of string))))

(defmethod enable ((mode certificate-exception-mode) &key)
  (setf (certificate-exceptions (buffer mode)) (certificate-exceptions mode)))

(defmethod disable ((mode certificate-exception-mode) &key)
  (setf (certificate-exceptions (buffer mode)) nil))

(define-command add-domain-to-certificate-exceptions (&key (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate exception list.
This is only effective if `certificate-exception-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-exceptions*' in your config file:

\(setf nyxt/mode/certificate-exception:*default-certificate-exceptions*
      '(\"nyxt.atlas.engineer\" \"example.org\"))"
  (if (find-submode 'certificate-exception-mode buffer)
      (let ((input (prompt1
                     :prompt "URL host to add to exception list"
                     :input (render-url (url buffer))
                     :sources (list
                               (make-instance 'prompter:raw-source
                                              :name "URL")
                               (make-instance 'nyxt/mode/history:history-all-source
                                              :buffer buffer)))))
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

(defmethod nyxt:default-modes append ((buffer web-buffer))
  '(certificate-exception-mode))

(defmethod nyxt:default-modes append ((buffer nosave-buffer))
  '(certificate-exception-mode))
