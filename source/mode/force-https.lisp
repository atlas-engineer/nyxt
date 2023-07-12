;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/force-https
  (:documentation "Package for `force-https-mode', mode to redirect all HTTP traffic to HTTPS."))
(in-package :nyxt/mode/force-https)

;; TODO: Add style to loop help page?
(defun https->http-loop-help (buffer url) ; TODO: Factor with tls-help?
  "This function is invoked upon HTTPS->HTTP->HTTPS loops to give users
help on how to proceed."
  (setf (nyxt::status buffer) :failed)
  (nyxt::html-set
   (spinneret:with-html-string
     (:nstyle (style buffer))
     (:h1 (format nil "HTTPS → HTTP loop: ~a" (render-url url)))
     (:p "The HTTPS address you are trying to visit redirects to HTTP while the "
         (:code "force-https-mode") " is on.")
     (:p "Since HTTP connections are not secure,"
         " it's not recommended to proceed if you don't trust the target host.")
     (:p " If you really want to proceed, you can either:"
         (:ul
          (:li "disable "
               (:code "force-https-mode") " temporarily;")
          (:li "or disable it dynamically with auto-rules' "
               (:code "save-exact-modes-for-future-visits") "."))))
   buffer))

(define-mode force-https-mode ()
  "Redirect HTTP traffic to HTTPS.

Notice that it may break websites whose certificates are not known, or those who
don't have an HTTPS version.

To permanently bypass the \"Unacceptable TLS Certificate\" error:
\(setf nyxt/mode/certificate-exception:*default-certificate-exceptions*
       '(\"your.unacceptable.cert.website\"))

Example:

\(define-configuration web-buffer
  ((default-modes (append '(force-https-mode) %slot-default%))))"
  ((previous-url (quri:uri ""))))

(defun force-https-handler (request-data)
  "Impose HTTPS on any link with HTTP scheme."
  (let ((url (url request-data))
        (mode (find-submode 'force-https-mode (buffer request-data))))
    (cond
      ((string/= (quri:uri-scheme url) "http")
       request-data)
      ((quri:uri= (previous-url mode) url)
       (log:info "HTTPS->HTTP redirection loop detected, stop forcing '~a'" url)
       (https->http-loop-help (buffer request-data) url)
       nil)
      (t
       ;; Warning: Copy URL, else next line would modify the scheme of
       ;; `previous-url' as well.
       (setf (previous-url mode) (quri:copy-uri url))
       (log:info "HTTPS enforced on '~a'" (render-url url))
       ;; FIXME: http-only websites are displayed as "https://foo.bar"
       ;; FIXME: some websites (e.g., go.com) simply time-out
       (setf (url request-data)
             (quri:copy-uri url :scheme "https"
                                :port (quri.port:scheme-default-port "https")))
       request-data))))

(defmethod enable ((mode force-https-mode) &key)
  (hooks:add-hook (request-resource-hook (buffer mode)) 'force-https-handler))

(defmethod disable ((mode force-https-mode) &key)
  (hooks:remove-hook (request-resource-hook (buffer mode)) 'force-https-handler))

(defmethod on-signal-load-finished ((mode force-https-mode) url)
  (declare (ignore url))
  (when (eq (slot-value (buffer mode) 'nyxt::status) :finished)
    (setf (previous-url mode) (quri:uri "")))
  nil)
