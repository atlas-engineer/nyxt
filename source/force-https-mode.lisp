(uiop:define-package :nyxt/force-https-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for enforcing HTTPS on any URL clicked/hinted/set by user."))
(in-package :nyxt/force-https-mode)

(defun force-https-handler (resource)
  "Imposes HTTPS on any link with HTTP scheme."
  (let ((uri (quri:uri (url resource))))
    (if (string= (quri:uri-scheme uri) "http")
        (progn (log:info "HTTPS enforced on ~a" (quri:render-uri uri))
               ;; FIXME: http-only websites are displayed as "https://foo.bar"
               ;; FIXME: some websites (e.g., go.com) simply time-out
               (setf (quri:uri-scheme uri) "https"
                     (quri:uri-port uri) (quri.port:scheme-default-port "https")
                     (url resource) (quri:render-uri uri)))))
  (values resource :forward))

(define-mode force-https-mode ()
  "Impose HTTPS on any link being set. Use at your own risk --
can break websites whose certificates are not known
and websites that still don't have HTTPS version (shame on them!).

To escape \"Unacceptable TLS Certificate\" error:
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
