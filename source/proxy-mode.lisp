;; proxy-mode.lisp --- Proxy mode (e.g. Tor et al.)

(in-package :next)

(define-mode proxy-mode ()
    "Enable forwarding of all network requests to a specific host.
This can apply to specific buffer."
    ((server-address :accessor server-address :initarg :server-address
                     :initform "socks://127.0.0.1:9050"
                     :documentation "The address of the proxy server.
It's made of three components: protocol, host and port.
Example:
  http://192.168.1.254:8080")
     (whitelist :accessor whitelist :initarg :whitelist
                :initform '("localhost" "localhost:8080")
                :documentation "A list of URI not to forward to the proxy.
It must be a list of strings.")
     (destructor
      :initform
      (lambda (mode)
        (%%set-proxy *interface* (buffer mode) "" nil)
        (echo "Proxy mode disabled."))))
  (%%set-proxy *interface* (buffer %mode) (server-address %mode) (whitelist %mode))
  (echo "Proxy set to ~a (whitelisting ~a)." (server-address %mode) (whitelist %mode)))
