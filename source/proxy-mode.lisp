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
  ((proxy (make-instance 'proxy
                         :server-address (quri:uri \"socks5://localhost:9050\")
                         :allowlist '(\"localhost\" \"localhost:8080\")
                         :proxied-downloads-p t))))

\(define-configuration buffer
  ((default-modes (append '(proxy-mode) %slot-default))))"
  ((proxy (make-instance 'proxy
                 :server-address (quri:uri "socks5://localhost:9050")
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
            (object-display (server-address (proxy mode)))
            (allowlist (proxy mode)))))))
