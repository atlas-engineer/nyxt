;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/proxy-mode
    (:use :common-lisp :trivia :nyxt)
  (:documentation "Proxy mode (e.g. Tor et al.)"))
(in-package :nyxt/proxy-mode)

(define-mode proxy-mode ()
  "Enable forwarding of all network requests to a specific host.
As for every mode, it only applies to the current buffer.  If you want to enable
a proxy for all buffers, add it to the list of default modes.

Example to use Tor as a proxy both for browsing and downloading:

\(define-configuration nyxt/proxy-mode:proxy-mode
  ((nyxt/proxy-mode:proxy (make-instance 'proxy
                                         :url (quri:uri \"socks5://localhost:9050\")
                                         :allowlist '(\"localhost\" \"localhost:8080\")
                                         :proxied-downloads-p t))))

(defmethod initialize-modes :after ((buffer buffer))
  (make-mode 'proxy-mode buffer))"
  ((proxy (make-instance 'proxy
                 :url (quri:uri "socks5://localhost:9050")
                 :allowlist '("localhost" "localhost:8080")
                 :proxied-downloads-p t)
          :type proxy)
   (destructor
    (lambda (mode)
      (setf (proxy (buffer mode)) nil)))
   (constructor
    (lambda (mode)
      (setf (proxy (buffer mode)) (proxy mode))
      (echo "Proxy set to ~a (allowlisting ~a)."
            (render-url (url (proxy mode)))
            (allowlist (proxy mode)))))))
