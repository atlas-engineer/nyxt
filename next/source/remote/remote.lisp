;;; remote.lisp --- remote gui interface

(in-package :next)

;; expose push-key-chord to server endpoint
(import 'push-key-chord :s-xml-rpc-exports)

(defclass remote-interface ()
  ((host :accessor host :initform "localhost")
   (port :accessor port :initform 8082)
   (url :accessor url :initform "/RPC2")))

(defmethod start-interface ((interface remote-interface))
  (s-xml-rpc:start-xml-rpc-server :port 8081))

(defmethod kill-interface ((interface remote-interface)))

(defmethod window-make ((interface remote-interface))
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.make")
     :host host :port port :url url)))

(defmethod window-delete ((interface remote-interface) window)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.delete" window)
     :host host :port port :url url)))

(defmethod window-switch ((interface remote-interface) window)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.switch" window)
     :host host :port port :url url)))

(defmethod minibuffer-set-height ((interface remote-interface) window height)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "minibuffer.set.height" window height)
     :host host :port port :url url)))

(defmethod minibuffer-execute-javascript ((interface remote-interface) window javascript)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "minibuffer.execute.javascript" window javascript)
     :host host :port port :url url)))

(defmethod buffer-make ((interface remote-interface))
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "buffer.make")
     :host host :port port :url url)))

(defmethod set-visible-buffer-for-pane ((interface remote-interface) view)
  (declare (ignore view)))

(defmethod delete-view ((interface remote-interface) view)
  (declare (ignore view)))

(defmethod web-view-set-url ((interface remote-interface) view url)
  (declare (ignore view url)))

(defmethod web-view-get-url ((interface remote-interface) view)
  (declare (ignore view)))

(defmethod web-view-execute ((interface remote-interface) view script &optional callback)
  (declare (ignore view script callback)))

(defmethod web-view-set-url-loaded-callback ((interface remote-interface) view function)
  (declare (ignore view function)))

(defmethod copy ((interface remote-interface)))
(defmethod paste ((interface remote-interface)))
(defmethod cut ((interface remote-interface)))
