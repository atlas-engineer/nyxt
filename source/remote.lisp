;;; remote.lisp --- remote gui interface

(in-package :next)

(defvar *interface* nil
  "The CLOS object responsible for rendering the interface.")

(defclass window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer)
   (minibuffer-callbacks :accessor minibuffer-callbacks :initform (make-hash-table :test #'equal))))

(defclass buffer ()
  ((id :accessor id :initarg :id)
   (name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode)
   (view :accessor view :initarg :view)
   (modes :accessor modes :initarg :modes)
   (callbacks :accessor callbacks :initform (make-hash-table :test #'equal))))

(defclass remote-interface ()
  ((host :accessor host :initform "localhost")
   (active-connection :accessor active-connection :initform nil)
   (port :accessor port :initform 8082)
   (url :accessor url :initform "/RPC2")
   (windows :accessor windows :initform (make-hash-table :test #'equal))
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))))

(defmethod start-interface ((interface remote-interface))
  "Start the XML RPC Server."
  (setf (active-connection interface)
        (s-xml-rpc:start-xml-rpc-server :port 8081)))

(defmethod kill-interface ((interface remote-interface))
  "Kill the XML RPC Server."
  (when (active-connection interface)
    (s-xml-rpc:stop-server (active-connection interface))))

(defmethod window-make ((interface remote-interface))
  "Create a window and return the window object."
  (with-slots (host port url windows) interface
    (let* ((window-id (s-xml-rpc:xml-rpc-call
                       (s-xml-rpc:encode-xml-rpc-call "window.make")
                       :host host :port port :url url))
           (window (make-instance 'window :id window-id)))
      (setf (gethash window-id windows) window)
      window)))

(defmethod window-delete ((interface remote-interface) (window window))
  "Delete a window object and remove it from the hash of windows."
  (with-slots (host port url windows) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "window.delete" (id window))
     :host host :port port :url url)
    (remhash (id window) windows)))

(defmethod window-active ((interface remote-interface))
  "Return the window object for the currently active window."
  (with-slots (host port url windows) interface
    (gethash (s-xml-rpc:xml-rpc-call
              (s-xml-rpc:encode-xml-rpc-call "window.active")
              :host host :port port :url url)
             windows)))

(defmethod window-set-active-buffer ((interface remote-interface)
                                     (window window)
                                     (buffer buffer))
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call
      "window.set.active.buffer" (id window) (id buffer))
     :host host :port port :url url)
    (setf (active-buffer window) buffer)))

(defmethod window-active-buffer ((interface remote-interface) window)
  "Return the active buffer for a given window."
  (active-buffer window))

(defmethod buffer-make ((interface remote-interface))
  (with-slots (host port url buffers) interface
    (let* ((buffer-id (s-xml-rpc:xml-rpc-call
                       (s-xml-rpc:encode-xml-rpc-call "buffer.make")
                       :host host :port port :url url))
           (buffer (make-instance 'buffer :id buffer-id)))
      (setf (gethash buffer-id buffers) buffer)
      buffer)))

(defmethod buffer-delete ((interface remote-interface) (buffer buffer))
  (with-slots (host port url buffers) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "buffer.delete" (id buffer))
     :host host :port port :url url)
    (remhash (id buffer) buffers)))

(defmethod buffer-execute-javascript ((interface remote-interface)
                                      (buffer buffer) javascript &optional (callback nil))
  (with-slots (host port url) interface
    (let ((callback-id
            (s-xml-rpc:xml-rpc-call
             (s-xml-rpc:encode-xml-rpc-call "buffer.execute.javascript" (id buffer) javascript)
             :host host :port port :url url)))
      (setf (gethash callback-id (callbacks buffer)) callback)
      callback-id)))

(defmethod minibuffer-set-height ((interface remote-interface)
                                  (window window) height)
  (with-slots (host port url) interface
    (s-xml-rpc:xml-rpc-call
     (s-xml-rpc:encode-xml-rpc-call "minibuffer.set.height" (id window) height)
     :host host :port port :url url)))

(defmethod minibuffer-execute-javascript ((interface remote-interface)
                                          (window window) javascript &optional (callback nil))
  (with-slots (host port url) interface
    (let ((callback-id
            (s-xml-rpc:xml-rpc-call
             (s-xml-rpc:encode-xml-rpc-call "minibuffer.execute.javascript" (id window) javascript)
             :host host :port port :url url)))
      (setf (gethash callback-id (minibuffer-callbacks window)) callback)
      callback-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expose Lisp Core XML RPC Endpoints ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun buffer-javascript-call-back (buffer-id javascript-response callback-id)
  (let* ((buffer (gethash buffer-id (buffers *interface*)))
         (callback (gethash callback-id (callbacks buffer))))
    (when callback
      (funcall callback javascript-response))))

(defun minibuffer-javascript-call-back (window-id javascript-response callback-id)
  (let* ((window (gethash window-id (windows *interface*)))
         (callback (gethash callback-id (minibuffer-callbacks window))))
    (when callback
      (funcall callback javascript-response))))

(import 'push-key-chord :s-xml-rpc-exports)
(import 'buffer-javascript-call-back :s-xml-rpc-exports)
(import 'minibuffer-javascript-call-back :s-xml-rpc-exports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience methods and functions for Users of the API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod active-buffer ((interface remote-interface))
  (window-active-buffer interface (window-active interface)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHODS BELOW ARE NOT NECESSARY - TEMPORARY FOR COMPILATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod copy ((interface remote-interface)))
(defmethod paste ((interface remote-interface)))
(defmethod cut ((interface remote-interface)))
(defmethod web-view-set-url-loaded-callback ((interface remote-interface) view function)
  (declare (ignore view function)))
(defmethod web-view-set-url ((interface remote-interface) view url)
  (declare (ignore view url)))
(defmethod web-view-get-url ((interface remote-interface) view)
  (declare (ignore view)))
