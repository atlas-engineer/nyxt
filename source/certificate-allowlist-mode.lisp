(uiop:define-package :nyxt/certificate-allowlist-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Certificate allowlist mode"))
(in-package :nyxt/certificate-allowlist-mode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

(sera:export-always '*default-certificate-allowlist*)
(defparameter *default-certificate-allowlist* '()
  "List of hostnames for which to ignore certificate errors.
See the `add-domain-to-certificate-allowlist' command.")

(define-mode certificate-allowlist-mode ()
  "Enable ignoring of certificate errors.
This can apply to specific buffers.
See the `add-domain-to-certificate-allowlist' command."
  ((certificate-allowlist :initarg :certificate-allowlist
                          :accessor certificate-allowlist
                          :type list-of-strings
                          :initform *default-certificate-allowlist*)
   (destructor
    :initform
    (lambda (mode)
      (setf (certificate-allowlist (buffer mode)) nil)))
   (constructor
    :initform
    (lambda (mode)
      (setf (certificate-allowlist (buffer mode)) (certificate-allowlist mode))))))

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

(define-command add-domain-to-certificate-allowlist (&optional (buffer (current-buffer)))
  "Add the current hostname to the buffer's certificate allowlist.
This is only effective if `certificate-allowlist-mode' is enabled.

To make this change permanent, you can customize
`*default-certificate-allowlist*' in your init file:

\(setf nyxt/certificate-allowlist-mode:*default-certificate-allowlist*
      '(\"nyxt.atlas.engineer\" \"example.org\"))"
  (if (find-submode buffer 'certificate-allowlist-mode)
      (with-result (input (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "URL host to allowlist:"
                            :suggestion-function (previous-history-urls-suggestion-filter))))
        (unless (url-empty-p (url (htree:data input)))
          (let ((host (quri:uri-host (url (htree:data input)))))
            (echo "Allowlisted ~s." host)
            (pushnew host (certificate-allowlist buffer) :test #'string=))))
      (echo "Enable certificate-allowlist-mode first.")))

;; TODO: Implement command remove-domain-from-certificate-allowlist.
;;       Currently it is not possible due to WebKit limitations.
