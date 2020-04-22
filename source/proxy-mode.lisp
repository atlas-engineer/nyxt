(uiop:define-package :next/proxy-mode
    (:use :common-lisp :trivia :next)
  (:documentation "Proxy mode (e.g. Tor et al.)"))
(in-package :next/proxy-mode)

(trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)

(serapeum:export-always '*default-proxy*)
(defparameter *default-proxy*
  (make-instance next::*proxy-class*
                 :server-address "socks5://127.0.0.1:9050"
                 :whitelist '("localhost" "localhost:8080")
                 :proxied-downloads-p t))

(define-mode proxy-mode ()
  "Enable forwarding of all network requests to a specific host.
As for every mode, it only applies to the current buffer.  If you want to enable
a proxy for all buffers, add it to the list of default modes.

Example:

\(define-configuration buffer
  ((default-modes (append '(proxy-mode) %slot-default))))"
  ((proxy :initarg :proxy
          :accessor proxy
          :type proxy
          :initform *default-proxy*)
   (destructor
    :initform
    (lambda (mode)
      (setf (proxy (buffer mode)) nil)))
   (constructor
    :initform
    (lambda (mode)
      (setf (proxy (buffer mode)) (proxy mode))
      (echo "Proxy set to ~a (whitelisting ~a) for ~a."
            (server-address (proxy mode))
            (whitelist (proxy mode))
            (buffer mode))))))
