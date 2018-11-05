;;; remote.lisp --- remote gui interface

(in-package :next)

;; expose push-key-chord to server endpoint
(import 'push-key-chord :s-xml-rpc-exports)

(defclass window ()
  ((id :accessor id)))

(defclass remote-interface ()
  ((host :accessor host :initform "localhost")
   (active-connection :accessor active-connection :initform nil)
   (port :accessor port :initform 8082)
   (url :accessor url :initform "/RPC2")
   (windows :accessor windows :initform (make-hash-table))
   (buffers :accessor buffers :initform (make-hash-table))))

(defmethod start-interface ((interface remote-interface))
  (setf (active-connection interface)
        (s-xml-rpc:start-xml-rpc-server :port 8081)))

(defmethod kill-interface ((interface remote-interface))
  (when (active-connection interface)
    (s-xml-rpc:stop-server (active-connection interface))))

(defmethod window-make ((interface remote-interface))
  (with-slots (host port url windows) interface
    (let ((window (s-xml-rpc:xml-rpc-call
                   (s-xml-rpc:encode-xml-rpc-call "window.make")
                   :host host :port port :url url)))
      (setf (gethash window windows) (make-instance 'window))
      window)))

(defmethod window-delete ((interface remote-interface) window)
  (with-slots (host port url windows) interface
    (setf (gethash window windows) nil)
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.delete" window)
     :host host :port port :url url)))

(defmethod window-active ((interface remote-interface))
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.active")
     :host host :port port :url url)))

(defmethod window-set-visible-buffer ((interface remote-interface) buffer window)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.set.visible.buffer" buffer window)
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

(defmethod buffer-delete ((interface remote-interface) buffer)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "buffer.delete" buffer)
     :host host :port port :url url)))

(defmethod web-view-execute ((interface remote-interface) view script &optional callback)
  (declare (ignore view script callback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHODS BELOW ARE NOT NECESSARY - TEMPORARY FOR COMPILATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod web-view-set-url-loaded-callback ((interface remote-interface) view function)
  (declare (ignore view function)))

(defmethod web-view-set-url ((interface remote-interface) view url)
  (declare (ignore view url)))

(defmethod web-view-get-url ((interface remote-interface) view)
  (declare (ignore view)))

(defmethod copy ((interface remote-interface)))
(defmethod paste ((interface remote-interface)))
(defmethod cut ((interface remote-interface)))
