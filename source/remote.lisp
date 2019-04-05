;;; remote.lisp --- remote gui interface

(in-package :next)

(defvar *interface* nil
  "The CLOS object responsible for rendering the interface.")

(defclass window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer :initform nil)
   (minibuffer-active :accessor minibuffer-active :initform nil)
   (minibuffer-callbacks :accessor minibuffer-callbacks
                         :initform (make-hash-table :test #'equal))))

(defclass buffer ()
  ((id :accessor id :initarg :id)
   (name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode)
   (view :accessor view :initarg :view)
   (modes :accessor modes :initarg :modes)
   (callbacks :accessor callbacks
              :initform (make-hash-table :test #'equal))))

(defmethod did-commit-navigation ((buffer buffer) url)
  (setf (name buffer) url)
  (did-commit-navigation (mode buffer) url))

(defmethod did-finish-navigation ((buffer buffer) url)
  (did-finish-navigation (mode buffer) url))

(defclass remote-interface ()
  ((platform-port :accessor platform-port :initform *platform-port-socket*)
   (active-connection :accessor active-connection :initform nil)
   (url :accessor url :initform "/RPC2")
   (windows :accessor windows :initform (make-hash-table :test #'equal))
   (total-window-count :accessor total-window-count :initform 0)
   (last-active-window :accessor last-active-window :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :accessor total-buffer-count :initform 0)))

(defmethod host ((interface remote-interface))
  "Retrieve the host of the platform port dynamically.
It's important that it is dynamic since the platform port can be reconfigured on
startup after the remote-interface was set up."
  (getf (platform-port interface) :host))

(defmethod port ((interface remote-interface))
  "Retrieve the port of the platform port dynamically.
It's important that it is dynamic since the platform port can be reconfigured on
startup after the remote-interface was set up."
  (getf (platform-port interface) :port))

(defmethod start-interface ((interface remote-interface))
  "Start the XML RPC Server."
  (setf (active-connection interface)
        (handler-case
            (s-xml-rpc:start-xml-rpc-server :port *core-port*)
          (SB-BSD-SOCKETS:ADDRESS-IN-USE-ERROR ()
            (let ((url-list (or *free-args* (list *default-new-buffer-url*))))
              (format *error-output* "Port ~a already in use, requesting to open URL(s) ~a.~%"
                      *core-port* url-list)
              ;; TODO: Check for errors (S-XML-RPC:XML-RPC-FAULT).
              (handler-case
                  (progn
                    (s-xml-rpc:xml-rpc-call
                     (s-xml-rpc:encode-xml-rpc-call "MAKE-BUFFERS" url-list)
                     :port *core-port*)
                    (uiop:quit))
                (error (c)
                  (declare (ignore c))
                  (let ((new-port (find-port:find-port)))
                    (format *error-output* "Port ~a does not seem to be used by Next, trying ~a instead.~%"
                            *core-port* new-port)
                    (setf *core-port* new-port)
                    (s-xml-rpc:start-xml-rpc-server :port *core-port*)))))))))

(defmethod kill-interface ((interface remote-interface))
  "Kill the XML RPC Server."
  (when (active-connection interface)
    (log:debug "Stopping server")
    (s-xml-rpc:stop-server (active-connection interface))))

(defmethod %xml-rpc-send ((interface remote-interface) (method string) &rest args)
  ;; TODO: Make %xml-rpc-send asynchronous?
  ;; If the platform port ever hangs, the next %xml-rpc-send will hang the Lisp core too.
  (with-slots (url) interface
    (s-xml-rpc:xml-rpc-call
     (apply #'s-xml-rpc:encode-xml-rpc-call method args)
     :host (host interface) :port (port interface) :url url)))

(defmethod get-unique-window-identifier ((interface remote-interface))
  (incf (total-window-count interface))
  (format nil "~a" (total-window-count interface)))

(defmethod get-unique-buffer-identifier ((interface remote-interface))
  (incf (total-buffer-count interface))
  (format nil "~a" (total-buffer-count interface)))

(defmethod window-make ((interface remote-interface))
  "Create a window and return the window object."
  (let* ((window-id (get-unique-window-identifier interface))
         (window (make-instance 'window :id window-id)))
    (setf (gethash window-id (windows interface)) window)
    (%xml-rpc-send interface "window.make" window-id)
    window))

(defmethod window-set-title ((interface remote-interface) (window window) title)
  "Set the title for a given window."
  (%xml-rpc-send interface "window.set.title" (id window) title))

(defmethod window-delete ((interface remote-interface) (window window))
  "Delete a window object and remove it from the hash of windows."
  (%xml-rpc-send interface "window.delete" (id window))
  (with-slots (windows) interface
    (remhash (id window) windows)))

(defmethod window-active ((interface remote-interface))
  "Return the window object for the currently active window."
  (with-slots (windows) interface
    (let ((window (gethash (%xml-rpc-send interface "window.active")
                           windows)))
      (when window
        (setf (last-active-window interface) window))
      (last-active-window interface))))

(defmethod window-exists ((interface remote-interface) (window window))
  "Return if a window exists."
  (%xml-rpc-send interface "window.exists" (id window)))

(defmethod %window-set-active-buffer ((interface remote-interface)
                                      (window window)
                                      (buffer buffer))
  (%xml-rpc-send interface "window.set.active.buffer" (id window) (id buffer))
  (setf (active-buffer window) buffer))

(defmethod window-set-active-buffer ((interface remote-interface)
                                     (window window)
                                     (buffer buffer))
  ;; TODO: Replace this swapping business with a simple swap + a "refresh rendering" RPC call?
  (let ((window-with-same-buffer (find-if
                                  (lambda (other-window) (and (not (eq other-window window))
                                                              (eql (active-buffer other-window) buffer)))
                                  (alexandria:hash-table-values (windows *interface*)))))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (buffer-make *interface*))
              (buffer-swap (active-buffer window)))
          (log:debug "Swapping with buffer from existing window.")
          (%window-set-active-buffer interface window-with-same-buffer temp-buffer)
          (%window-set-active-buffer interface window buffer)
          (%window-set-active-buffer interface window-with-same-buffer buffer-swap)
          (buffer-delete interface temp-buffer))
        (%window-set-active-buffer interface window buffer))))

(defmethod window-active-buffer ((interface remote-interface) window)
  "Return the active buffer for a given window."
  (active-buffer window))

(defmethod window-set-minibuffer-height ((interface remote-interface)
                                         window height)
  (%xml-rpc-send interface "window.set.minibuffer.height" (id window) height))

(defmethod buffer-make ((interface remote-interface))
  (let* ((buffer-id (get-unique-buffer-identifier interface))
         (cookies-path (namestring (merge-pathnames *cookie-path-dir* "cookies.txt")))
         (buffer (make-instance 'buffer :id buffer-id)))
    (setf (gethash buffer-id (buffers interface)) buffer)
    (%xml-rpc-send interface "buffer.make" buffer-id
                   (list
                    :cookies-path cookies-path))
    buffer))

(defmethod %buffer-make ((interface remote-interface)
                         &optional
                           (name "default")
                           (mode (funcall *default-new-buffer-mode*)))
  (let ((buffer (buffer-make interface)))
    (setf (name buffer) name)
    (setf (mode buffer) mode)
    (setup mode buffer)
    buffer))

(defmethod %get-inactive-buffer ((interface remote-interface))
  (let ((active-buffers
          (mapcar #'active-buffer
                      (alexandria:hash-table-values (windows *interface*))))
        (buffers (alexandria:hash-table-values (buffers *interface*))))
    (first (set-difference buffers active-buffers))))

(defmethod buffer-delete ((interface remote-interface) (buffer buffer))
  (let ((parent-window (find-if
                        (lambda (window) (eql (active-buffer window) buffer))
                        (alexandria:hash-table-values (windows *interface*))))
        (replacement-buffer (or (%get-inactive-buffer interface)
                                (%buffer-make interface))))
    (%xml-rpc-send interface "buffer.delete" (id buffer))
    (when parent-window
      (window-set-active-buffer interface parent-window replacement-buffer))
    (with-slots (buffers) interface
      (remhash (id buffer) buffers))))

(defmethod buffer-evaluate-javascript ((interface remote-interface)
                                       (buffer buffer) javascript &optional (callback nil))
  (let ((callback-id
          (%xml-rpc-send interface "buffer.evaluate.javascript" (id buffer) javascript)))
    (setf (gethash callback-id (callbacks buffer)) callback)
    callback-id))

(defmethod minibuffer-evaluate-javascript ((interface remote-interface)
                                           (window window) javascript &optional (callback nil))
  (let ((callback-id
          (%xml-rpc-send interface "minibuffer.evaluate.javascript" (id window) javascript)))
    (setf (gethash callback-id (minibuffer-callbacks window)) callback)
    callback-id))

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

(defun buffer-did-commit-navigation (buffer-id url)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-commit-navigation buffer url)))

(defun buffer-did-finish-navigation (buffer-id url)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-finish-navigation buffer url)))

(defun window-will-close (window-id)
  (let ((windows (windows *interface*)))
    (log:debug "Closing window ID ~a (new total: ~a)" window-id
               (1- (length (alexandria:hash-table-values windows))))
    (remhash window-id windows)))

(defun make-buffers (urls)
  "Create new buffers from URLs."
  ;; The new active buffer should be the first created buffer.
  (when urls
    (let ((buffer (make-buffer))
          (window (window-make *interface*)))
      (set-url-buffer (car urls) buffer)
      (window-set-active-buffer *interface* window buffer))
    (loop for url in (cdr urls) do
      (let ((buffer (make-buffer)))
        (set-url-buffer url buffer)))))

(import 'buffer-did-commit-navigation :s-xml-rpc-exports)
(import 'buffer-did-finish-navigation :s-xml-rpc-exports)
(import 'push-key-event :s-xml-rpc-exports)
(import 'consume-key-sequence :s-xml-rpc-exports)
(import 'buffer-javascript-call-back :s-xml-rpc-exports)
(import 'minibuffer-javascript-call-back :s-xml-rpc-exports)
(import 'window-will-close :s-xml-rpc-exports)
(import 'make-buffers :s-xml-rpc-exports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience methods and functions for Users of the API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod active-buffer ((interface remote-interface))
  "Get the active buffer for the active window."
  (window-active-buffer interface (window-active interface)))

;; TODO: Prevent setting the minibuffer as the active buffer.
(defmethod set-active-buffer ((interface remote-interface)
                              (buffer buffer))
  "Set the active buffer for the active window."
  (let ((window-active (window-active interface)))
    (window-set-active-buffer interface window-active buffer)
    (setf (active-buffer window-active) buffer)))
