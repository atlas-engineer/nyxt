(uiop:define-package :nyxt/certificate-exception-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Certificate excetption mode"))
(in-package :nyxt/certificate-exception-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(sera:export-always '*default-certificate-exception*)
(defparameter *default-certificate-exception* '()
  "List of hostnames for which to ignore certificate errors.
See the `add-domain-to-certificate-exception' command.")

(define-mode certificate-exception-mode ()
  "Enable ignoring of certificate errors.
This can apply to specific buffers.
See the `add-domain-to-certificate-exception' command."
  ((certificate-exception :initarg :certificate-exception
                          :accessor certificate-exception
                          :type list-of-strings
                          :initform *default-certificate-exception*)
   (destructor
    :initform
    (lambda (mode)
      (setf (certificate-exception (buffer mode)) nil)))
   (constructor
    :initform
    (lambda (mode)
      (setf (certificate-exception (buffer mode)) (certificate-exception mode))))))

(defun previous-history-urls-suggestion-filter (&optional (mode (find-submode
                                                            (current-buffer)
                                                            'web-mode)))
  "Suggestion function over all parent URLs."
  (let ((parents (htree:parent-nodes (history mode))))
    (when (htree:current (history mode))
      (push (htree:current (history mode)) parents))
    (setf parents (remove-if #'url-empty-p parents :key (alex:compose #'url #'htree:data)))
    (lambda (minibuffer)
      (if parents
          (fuzzy-match (input-buffer minibuffer) parents)
          '()))))

(define-command add-domain-to-certificate-exception (&optional (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate exception list.
This is only effective if `certificate-exception-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-exception*' in your init file:

\(setf nyxt/certificate-exception-mode:*default-certificate-exception*
      '(\"nyxt.atlas.engineer\" \"example.org\"))"
  (if (find-submode buffer 'certificate-exception-mode)
      (with-result (input (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "URL host to exception list:"
                            :suggestion-function (previous-history-urls-suggestion-filter))))
        (unless (url-empty-p (url (htree:data input)))
          (let ((host (quri:uri-host (url (htree:data input)))))
            (echo "Added exception for ~s." host)
            (pushnew host (certificate-exception buffer) :test #'string=))))
      (echo "Enable certificate-exception-mode first.")))

;; TODO: Implement command remove-domain-from-certificate-exception.
;;       Currently it is not possible due to WebKit limitations.
