(uiop:define-package :nyxt/certificate-whitelist-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Certificate whitelist mode"))
(in-package :nyxt/certificate-whitelist-mode)
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

(defun previous-history-urls-suggestion-filter (&optional (mode (find-submode
                                                            (current-buffer)
                                                            'web-mode)))
  "Suggestion function over all parent URLs."
  (let ((parents (htree:parent-nodes (history mode))))
    (push (htree:current (history mode)) parents)
    (setf parents (remove-if #'str:emptyp parents :key (alex:compose  #'url #'htree:data)))
    (lambda (minibuffer)
      (if parents
          (fuzzy-match (input-buffer minibuffer) parents)
          (error "Cannot navigate backwards.")))))

(define-command add-domain-to-certificate-whitelist (&optional (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate whitelist.
This is only effective if `certificate-whitelist-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-whitelist*' in your init file:

\(setf nyxt/certificate-whitelist-mode:*default-certificate-whitelist*
      '(\"nyxt.atlas.engineer\" \"example.org\"))"
  (if (find-submode buffer 'certificate-whitelist-mode)
      (with-result (input (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "URL host to whitelist:"
                            :suggestion-function (previous-history-urls-suggestion-filter))))
        (unless (str:emptyp (url (htree:data input)))
          (let ((host (host (url (htree:data input)))))
            (echo "Whitelisted ~s." host)
            (pushnew host (certificate-whitelist buffer) :test #'string=))))
      (echo "Enable certificate-whitelist-mode first.")))

;; TODO: Implement command remove-domain-from-certificate-whitelist.
;;       Currently it is not possible due to WebKit limitations.
