;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/force-https-mode
  (:use :common-lisp :nyxt)
  (:documentation "Mode for enforcing HTTPS on any URL clicked/hinted/set by user."))
(in-package :nyxt/force-https-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks))

;; TODO: Add style to loop help page?
(defun https->http-loop-help (buffer url) ; TODO: Factor with tls-help?
  "This function is invoked upon TLS certificate errors to give users
help on how to proceed."
  (setf (slot-value buffer 'nyxt::load-status) :failed)
  (nyxt::html-set
   (markup:markup
    (:h1 (format nil "HTTPS → HTTP loop: ~a" (object-display url)))
    (:p "The HTTPS address you are trying to visit redirects to HTTP while the "
        (:code "force-https-mode") " is on.")
    (:p "Since HTTP connections are not secure,"
        " it's not recommended to proceed if you don't trust the target host.")
    (:p " If you really want to proceed, you can either:"
        (:ul
         (:li "disable "
              (:code "force-https-mode") " temporarily;")
         (:li "or disable it dynamically with " (:code "auto-mode") "'s "
              (:code "save-exact-modes-for-future-visits") "."))))
   buffer))

(defun force-https-handler (request-data)
  "Impose HTTPS on any link with HTTP scheme."
  (let ((uri (url request-data))
        (mode (find-submode (buffer request-data) 'force-https-mode)))
    (cond
      ((string/= (quri:uri-scheme uri) "http")
       request-data)
      ((quri:uri= (previous-url mode) uri)
       (log:info "HTTPS->HTTP redirection loop detected, stop forcing '~a'" uri)
       (https->http-loop-help (buffer request-data) uri)
       nil)
      (t
       ;; Warning: Copy URI, else next line would modify the scheme of
       ;; `previous-url' as well.
       (setf (previous-url mode) (quri:copy-uri uri))
       (log:info "HTTPS enforced on '~a'" (object-display uri))
       ;; FIXME: http-only websites are displayed as "https://foo.bar"
       ;; FIXME: some websites (e.g., go.com) simply time-out
       (setf (quri:uri-scheme uri) "https"
             (quri:uri-port uri) (quri.port:scheme-default-port "https")
             (url request-data) uri)
       request-data))))

(define-mode force-https-mode ()
  "Impose HTTPS on every queried URI.
Use at your own risk -- it can break websites whose certificates are not known
and websites that still don't have HTTPS version (shame on them!).

To permanently bypass the \"Unacceptable TLS Certificate\" error:
\(setf nyxt/certificate-exception-mode:*default-certificate-exceptions*
       '(\"your.unacceptable.cert.website\"))

Example:

\(define-configuration buffer
  ((default-modes (append '(force-https-mode) %slot-default))))"
  ((previous-url (quri:uri ""))
   (destructor
    (lambda (mode)
      (hooks:remove-hook (request-resource-hook (buffer mode))
                         'force-https-handler)))
   (constructor
    (lambda (mode)
      (hooks:add-hook (request-resource-hook (buffer mode))
                      (make-handler-resource #'force-https-handler))))))
