;;; remote.lisp --- remote gui interface

(in-package :next)

(defclass window ()
  ((id :accessor id :initarg :id)
   (active-buffer :accessor active-buffer :initform nil)
   (minibuffer-active :accessor minibuffer-active :initform nil)
   (minibuffer-callbacks :accessor minibuffer-callbacks
                         :initform (make-hash-table :test #'equal))))

(defclass buffer ()
  ((id :accessor id :initarg :id)
   (name :accessor name :initarg :name)
   (mode :accessor mode :initarg :mode
         ;; TODO: This is rather clunky.  Use separate slot for the class symbol?
         :initform 'document-mode
         :documentation "The :initform and :initarg must be the class symbol.
The mode is instantiated on buffer initialization.")
   (view :accessor view :initarg :view)
   (modes :accessor modes :initarg :modes)
   (resource-query-functions :accessor resource-query-functions
                             :initarg :resource-query-functions
                             :initform nil)
   (callbacks :accessor callbacks
              :initform (make-hash-table :test #'equal))
   (scroll-distance :accessor scroll-distance :initform 50
                    :documentation "The distance scroll-down or scroll-up will scroll.")
   (horizontal-scroll-distance :accessor horizontal-scroll-distance :initform 50
                               :documentation "Horizontal scroll distance. The
distance scroll-left or scroll-right will scroll.")
   (current-zoom-ratio :accessor current-zoom-ratio :initform 1.0
                       :documentation "The current zoom relative to the default zoom.")
   (zoom-ratio-step :accessor zoom-ratio-step :initform 0.2
                    :documentation "The step size for zooming in and out.")
   (zoom-ratio-min :accessor zoom-ratio-min :initform 0.2
                   :documentation "The minimum zoom ratio relative to the default.")
   (zoom-ratio-max :accessor zoom-ratio-max :initform 5.0
                   :documentation "The maximum zoom ratio relative to the default.")
   (zoom-ratio-default :accessor zoom-ratio-default :initform 1.0
                       :documentation "The default zoom ratio.")))

(defmethod initialize-instance :after ((buffer buffer) &key)
  (when (symbolp (mode buffer))
    (setf (mode buffer) (make-instance (mode buffer)))))

;; A struct used to describe a key-chord
(defstruct key-chord
  key-code
  key-string
  modifiers
  position
  low-level-data)

(defmethod did-commit-navigation ((buffer buffer) url)
  (setf (name buffer) url)
  (did-commit-navigation (mode buffer) url))

(defmethod did-finish-navigation ((buffer buffer) url)
  (did-finish-navigation (mode buffer) url))

(defclass remote-interface ()
  ((core-port :accessor core-port :initform 8081
              :documentation "The XML-RPC server port of the Lisp core.")
   (platform-port-socket :accessor platform-port-socket :initform '(:host "localhost" :port 8082)
                         :documentation "The XML-RPC remote socket of the platform-port.")
   (port :accessor port :initform (make-instance 'port)
         :documentation "The CLOS object responible for handling the platform port.")
   (active-connection :accessor active-connection :initform nil)
   (url :accessor url :initform "/RPC2")
   (minibuffer :accessor minibuffer :initform (make-instance 'minibuffer)
               :documentation "The minibuffer object.")
   (windows :accessor windows :initform (make-hash-table :test #'equal))
   (total-window-count :accessor total-window-count :initform 0)
   (last-active-window :accessor last-active-window :initform nil)
   (buffers :accessor buffers :initform (make-hash-table :test #'equal))
   (total-buffer-count :accessor total-buffer-count :initform 0)))

(defmethod host ((interface remote-interface))
  "Retrieve the host of the platform port dynamically.
It's important that it is dynamic since the platform port can be reconfigured on
startup after the remote-interface was set up."
  (getf (platform-port-socket interface) :host))

(defmethod host-port ((interface remote-interface))
  "Retrieve the port of the platform port dynamically.
It's important that it is dynamic since the platform port can be reconfigured on
startup after the remote-interface was set up."
  (getf (platform-port-socket interface) :port))

(defmethod initialize-instance :after ((interface remote-interface)
                                       &key &allow-other-keys)
  "Start the XML RPC Server."
  (setf (active-connection interface)
        ;; TODO: Ideally, s-xml-rpc should send an implementation-independent
        ;; condition.
        (handler-case
            (s-xml-rpc:start-xml-rpc-server :port (core-port interface))
          (#+sbcl sb-bsd-sockets:address-in-use-error
           #+ccl ccl:socket-error
           (#+ccl e
            )
            (when #+sbcl t
                  #+ccl (eq (ccl:socket-error-identifier e) :address-in-use)
              (let ((url-list (or *free-args* (list *default-new-buffer-url*))))
                (format *error-output* "Port ~a already in use, requesting to open URL(s) ~a.~%"
                        (core-port interface) url-list)
                ;; TODO: Check for errors (S-XML-RPC:XML-RPC-FAULT).
                (handler-case
                    (progn
                      (s-xml-rpc:xml-rpc-call
                       (s-xml-rpc:encode-xml-rpc-call "make.buffers" url-list)
                       :port (core-port interface))
                      (uiop:quit))
                  (error ()
                    (let ((new-port (find-port:find-port)))
                      (format *error-output* "Port ~a does not seem to be used by Next, trying ~a instead.~%"
                              (core-port interface) new-port)
                      (setf (core-port interface) new-port)
                      (s-xml-rpc:start-xml-rpc-server :port (core-port interface)))))))))))


(defmethod kill-interface ((interface remote-interface))
  "Kill the XML RPC Server."
  (when (active-connection interface)
    (log:debug "Stopping server")
    (s-xml-rpc:stop-server (active-connection interface))))

(defmethod %xml-rpc-send ((interface remote-interface) (method string) &rest args)
  ;; TODO: Make %xml-rpc-send asynchronous?
  ;; If the platform port ever hangs, the next %xml-rpc-send will hang the Lisp core too.
  (with-slots (url) interface
    (handler-case
        (s-xml-rpc:xml-rpc-call
         (apply #'s-xml-rpc:encode-xml-rpc-call method args)
         :host (host interface) :port (host-port interface) :url url)
      (s-xml-rpc:xml-rpc-fault (c)
        (log:warn "~a" c)
        (echo (minibuffer *interface*)
              (format nil "Platform port failed to respond to '~a': ~a" method c))
        (error c)))))

(defmethod list-methods ((interface remote-interface))
  "Return the unsorted list of XML-RPC methods supported by the platform port."
  (%xml-rpc-send interface "listMethods"))

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

(defmethod window-set-minibuffer-height ((interface remote-interface)
                                         window height)
  (%xml-rpc-send interface "window.set.minibuffer.height" (id window) height))

(defmethod buffer-make ((interface remote-interface)
                        &key name mode)
  (let* ((buffer-id (get-unique-buffer-identifier interface))
         (cookies-path (namestring (ensure-parent-exists
                                    (merge-pathnames *cookies-path* "cookies.txt"))))
         (buffer (apply #'make-instance 'buffer :id buffer-id
                        (append (when name `(:name ,name))
                                (when mode `(:mode ,mode))))))
    (setf (gethash buffer-id (buffers interface)) buffer)
    (%xml-rpc-send interface "buffer.make" buffer-id
                   (list
                    :cookies-path cookies-path))
    buffer))

(defmethod %buffer-make ((interface remote-interface)
                         &optional
                           (name "default")
                           mode)
  (let* ((buffer (buffer-make interface :name name :mode mode)))
    (when (mode buffer)
      (setup (mode buffer) buffer))
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

(defmethod buffer-load ((interface remote-interface) (buffer buffer) uri)
  (%xml-rpc-send interface "buffer.load" (id buffer) uri))

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

(defmethod generate-input-event ((interface remote-interface)
                                 (window window)
                                 (event key-chord))
  "For now, we only generate keyboard events.
In the future, we could also support other input device events such as mouse
events."
  (log:debug "Generate input ~a for window ~a"
             (list
              (key-chord-key-code event)
              (key-chord-modifiers event)
              (key-chord-low-level-data event)
              (key-chord-position event))
             (id window))
  (%xml-rpc-send interface "generate.input.event"
                 (id window)
                 (key-chord-key-code event)
                 (or (key-chord-modifiers event) (list ""))
                 (key-chord-low-level-data event)
                 (float (or (first (key-chord-position event)) -1.0))
                 (float (or (second (key-chord-position event)) -1.0))))

(defmethod set-proxy ((interface remote-interface) (buffer buffer)
                      &optional (proxy-uri "") (ignore-hosts (list nil)))
  (%xml-rpc-send interface "set.proxy" (list (id buffer))
                 (if (string= proxy-uri "")
                     "default"
                     "custom")
                 proxy-uri ignore-hosts))

(defmethod get-proxy ((interface remote-interface) (buffer buffer))
  (%xml-rpc-send interface "get.proxy" (id buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expose Lisp Core XML RPC Endpoints ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun |buffer.javascript.call.back| (buffer-id javascript-response callback-id)
  (let* ((buffer (gethash buffer-id (buffers *interface*)))
         (callback (gethash callback-id (callbacks buffer))))
    (when callback
      (funcall callback javascript-response))))

(defun |minibuffer.javascript.call.back| (window-id javascript-response callback-id)
  (let* ((window (gethash window-id (windows *interface*)))
         (callback (gethash callback-id (minibuffer-callbacks window))))
    (when callback
      (funcall callback javascript-response))))

(defun |buffer.did.commit.navigation| (buffer-id url)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-commit-navigation buffer url)))

(defun |buffer.did.finish.navigation| (buffer-id url)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (did-finish-navigation buffer url)))

(defun |window.will.close| (window-id)
  (let ((windows (windows *interface*)))
    (log:debug "Closing window ID ~a (new total: ~a)" window-id
               (1- (length (alexandria:hash-table-values windows))))
    (remhash window-id windows)))

(defun |make.buffers| (urls)
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

(defun |request.resource| (buffer-id url event-type is-new-window is-known-type
                           mouse-button modifiers)
  "Return whether URL should be loaded or not."
  (declare (ignore event-type))
  (log:debug "Mouse ~a, modifiers ~a" mouse-button modifiers)
  (let ((buffer (gethash buffer-id (buffers *interface*))))
    (cond
      (is-new-window
       (log:info "Load ~a in new window" url)
       (|make.buffers| url)
       0)
      ((not is-known-type)
       (log:info "Buffer ~a downloads ~a" buffer url)
       0)
      (t
       (log:info "Forwarding ~a back to platform port" url)
       1))
    ;; TODO: Move the cond to RESOURCE-QUERY-FUNCTIONS to let the user customize
    ;; the behaviour.
    ;; (if (loop for function in (resource-query-functions buffer)
    ;;           always (funcall function url buffer-id))
    ;;     1
    ;;     0)
    ))

(import '|buffer.did.commit.navigation| :s-xml-rpc-exports)
(import '|buffer.did.finish.navigation| :s-xml-rpc-exports)
(import '|push.input.event| :s-xml-rpc-exports)
(import '|consume.key.sequence| :s-xml-rpc-exports)
(import '|buffer.javascript.call.back| :s-xml-rpc-exports)
(import '|minibuffer.javascript.call.back| :s-xml-rpc-exports)
(import '|window.will.close| :s-xml-rpc-exports)
(import '|make.buffers| :s-xml-rpc-exports)
(import '|request.resource| :s-xml-rpc-exports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience methods and functions for Users of the API ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod active-buffer ((interface remote-interface))
  "Get the active buffer for the active window."
  (active-buffer (window-active interface)))

;; TODO: Prevent setting the minibuffer as the active buffer.
(defmethod set-active-buffer ((interface remote-interface)
                              (buffer buffer))
  "Set the active buffer for the active window."
  (let ((window-active (window-active interface)))
    (window-set-active-buffer interface window-active buffer)
    (setf (active-buffer window-active) buffer)))
