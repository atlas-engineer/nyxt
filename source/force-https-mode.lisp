(uiop:define-package :nyxt/force-https-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for enforcing HTTPS on any URL clicked/hinted/set by user."))
(in-package :nyxt/force-https-mode)

(defun force-https-handler (request-data)
  "Impose HTTPS on any link with HTTP scheme."
  (let ((uri (url request-data)))
    (when (string= (quri:uri-scheme uri) "http")
      (log:info "HTTPS enforced on \"~a\"" (object-display uri))
      ;; FIXME: http-only websites are displayed as "https://foo.bar"
      ;; FIXME: some websites (e.g., go.com) simply time-out
      (setf (quri:uri-scheme uri) "https"
            (quri:uri-port uri) (quri.port:scheme-default-port "https")
            (url request-data) uri)))
  request-data)

(define-mode force-https-mode ()
  "Impose HTTPS on every queried URI.
Use at your own risk -- it can break websites whose certificates are not known
and websites that still don't have HTTPS version (shame on them!).

To permanently bypass the \"Unacceptable TLS Certificate\" error:
\(setf nyxt/certificate-whitelist-mode:*default-certificate-whitelist*
       '(\"your.unacceptable.cert.website\"))

Example:

\(define-configuration buffer
  ((default-modes (append '(force-https-mode) %slot-default))))"
  ((destructor
    :initform
    (lambda (mode)
      (hooks:remove-hook (request-resource-hook (buffer mode))
                         'force-https-handler)))
   (constructor
    :initform
    (lambda (mode)
      (hooks:add-hook (request-resource-hook (buffer mode))
                      (make-handler-resource #'force-https-handler))))))
