;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/certificate-exception-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Certificate excetption mode"))
(in-package :nyxt/certificate-exception-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

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

(defun previous-history-urls-suggestion-filter (&optional (mode (find-submode
                                                            (current-buffer)
                                                            'web-mode)))
  "Suggestion function over all parent URLs."
  (let ((parents (htree:all-parents (history mode))))
    (when (htree:current-owner-node (history mode))
      (push (htree:current-owner-node (history mode)) parents))
    (setf parents (remove-if #'url-empty-p parents :key (alex:compose #'url #'htree:data)))
    (lambda (minibuffer)
      (if parents
          (fuzzy-match (input-buffer minibuffer) parents)
          '()))))

(define-command add-domain-to-certificate-exceptions (&optional (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate exception list.
This is only effective if `certificate-exception-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-exceptions*' in your init file:

\(setf nyxt/certificate-exception-mode:*default-certificate-exceptions*
      '(\"nyxt.atlas.engineer\" \"example.org\"))"
  (if (find-submode buffer 'certificate-exception-mode)
      (let ((input (prompt-minibuffer
                    :input-prompt "URL host to exception list:"
                    :suggestion-function (previous-history-urls-suggestion-filter))))
        (unless (url-empty-p (url (htree:data input)))
          (let ((host (quri:uri-host (url (htree:data input)))))
            (echo "Added exception for ~s." host)
            (pushnew host (certificate-exceptions buffer) :test #'string=))))
      (echo "Enable certificate-exception-mode first.")))

;; TODO: Implement command remove-domain-from-certificate-exceptions.
;;       Currently it is not possible due to WebKit limitations.
