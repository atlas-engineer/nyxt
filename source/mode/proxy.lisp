;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/proxy
  (:shadow #:proxy) ; Because of exported slot of the same below.
  (:documentation "Proxy mode can be used to do all the networking (optionally
including downloads) via a proxy server.
It can also be configured to forward all the networking through local services
like Tor."))
(in-package :nyxt/mode/proxy)

(define-mode proxy-mode ()
  "Enable forwarding of all network requests to a specific host.
As for every mode, it only applies to the current buffer.  If you want to enable
a proxy for all buffers, add it to the list of default modes.

Example to use Tor as a proxy both for browsing and downloading:

\(define-configuration nyxt/mode/proxy:proxy-mode
  ((nyxt/mode/proxy:proxy (make-instance 'proxy
                                         :url (quri:uri \"socks5://localhost:9050\")
                                         :allowlist '(\"localhost\" \"localhost:8080\")
                                         :proxied-downloads-p t))))

\(define-configuration web-buffer
  ((default-modes (append '(proxy-mode) %slot-value%))))"
  ((proxy (make-instance 'nyxt:proxy
                         :url (quri:uri "socks5://localhost:9050")
                         :allowlist '("localhost" "localhost:8080")
                         :proxied-downloads-p t)
          :type nyxt:proxy)))

(defmethod enable ((mode proxy-mode) &key)
  (if (web-buffer-p (buffer mode))
      (progn
        (setf (nyxt:proxy (buffer mode)) (proxy mode))
        (echo "Buffer ~a proxy set to ~a, allowlisting ~a."
              (id (buffer mode))
              (render-url (url (proxy mode)))
              (allowlist (proxy mode))))
      (echo-warning "You cannot set the proxy for internal buffers.")))

(defmethod disable ((mode proxy-mode) &key)
  (when (web-buffer-p (buffer mode))
    (setf (nyxt:proxy (buffer mode)) nil)))
