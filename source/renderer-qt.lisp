;;; renderer-qt.lisp --- functions for creating Qt interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

(define-condition unsupported-operation (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Unsupported operation for this renderer."))))

@export
@export-accessors
(defclass qt-browser (browser)
  ((application :accessor application)))

(define-class-type browser)
(declaim (type (browser-type) *browser-class*))
@export
(defparameter *browser-class* 'qt-browser)

(defmethod initialize ((browser qt-browser) urls startup-timestamp)
  (log:debug "Initializing Qt Interface")
  (setf (application browser)
        (qt:new-q-application 1 (cffi:foreign-alloc :string
                                                    :initial-contents (list "Next")
                                                    :null-terminated-p t)))
  (finalize browser urls startup-timestamp)
  (qt:application-exec (application browser)))

(defmethod kill-interface ((browser qt-browser))
  (qt:application-quit (application browser)))

@export
@export-accessors
(defclass qt-window (window)
  ((qt-object :accessor qt-object)
   (box-layout :accessor box-layout)
   (minibuffer-view :accessor minibuffer-view)))

(define-class-type window)
(declaim (type (window-type) *window-class*))
@export
(defparameter *window-class* 'qt-window)

@export
@export-accessors
(defclass qt-buffer (buffer)
  ((qt-object :accessor qt-object)))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
@export
(defparameter *buffer-class* 'qt-buffer)

(defmethod initialize-instance :after ((window qt-window) &key)
  (with-slots (id qt-object box-layout active-buffer minibuffer-view) window
    (setf id (get-unique-window-identifier *browser*))
    (setf qt-object (qt:new-q-widget))
    (setf box-layout (qt:new-qv-box-layout))
    (setf active-buffer (make-instance *buffer-class*))
    (setf minibuffer-view (qt:new-q-web-engine-view))
    ;; Add views to window, configure window widget
    (qt:widget-set-layout qt-object box-layout)
    (qt:layout-add-widget box-layout (qt-object active-buffer))
    (qt:layout-add-widget box-layout minibuffer-view)
    (qt:layout-set-contents-margins box-layout 0 0 0 0)
    (qt:layout-set-spacing box-layout 0)
    (ipc-window-set-minibuffer-height window (status-buffer-height window))
    (qt:widget-resize qt-object 1024 768)
    (qt:widget-show qt-object)))

(defmethod process-destroy ((window qt-window))
  (window-delete window))

@export
(defmethod ipc-window-delete ((window qt-window))
  "Delete a window object and remove it from the hash of windows.")

(defmethod ipc-window-fullscreen ((window qt-window))
  (qt:window-show-full-screen (qt-object window)))

(defmethod ipc-window-unfullscreen ((window qt-window))
  (qt:window-show-normal (qt-object window)))

(defmethod initialize-instance :after ((buffer qt-buffer) &key)
  (next-hooks:run-hook (buffer-before-make-hook *browser*) buffer)
  (setf (id buffer) (get-unique-buffer-identifier *browser*))
  (setf (qt-object buffer) (qt:new-q-web-engine-view))
  (qt:web-engine-view-load (qt-object buffer) "https://www.example.com")
  (qt:load-started-listener-connect
   (qt-object buffer)
   (lambda ()
     (did-commit-navigation buffer (qt:web-engine-view-url (qt-object buffer)))))
  (qt:load-finished-listener-connect
   (qt-object buffer)
   (lambda ()
     (did-finish-navigation buffer (qt:web-engine-view-url (qt-object buffer))))))

@export
(defmethod ipc-window-make ((browser qt-browser))
  "Make a window."
  (make-instance *window-class*))

@export
(defmethod ipc-window-to-foreground ((window qt-window))
  "Show window in foreground.")

@export
(defmethod ipc-window-set-title ((window qt-window) title)
  "Set the title for a window."
  (qt:window-set-window-title (qt-object window) title)
  title)

@export
(defmethod ipc-window-active ((browser qt-browser))
  "Return the window object for the currently active window."
  (setf (last-active-window browser)
        (or (find-if #'qt:window-is-active-window (window-list) :key #'qt-object)
            (last-active-window browser))))

@export
(defmethod ipc-window-set-active-buffer ((window qt-window) (buffer qt-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (qt:widget-set-parent (qt-object (active-buffer window)) (cffi:null-pointer))
  (qt:layout-insert-widget (box-layout window) 0 (qt-object buffer))
  (qt:widget-show (qt-object buffer))
  (setf (active-buffer window) buffer))

@export
(defmethod ipc-window-set-minibuffer-height ((window qt-window) height)
  (qt:widget-set-fixed-height (minibuffer-view window) height))

@export
(defmethod ipc-buffer-make ((browser qt-browser) &key title default-modes)
  "Make buffer with title TITLE and modes DEFAULT-MODES."
  (apply #'make-instance *buffer-class*
         (append (when title `(:title ,title))
                 (when default-modes `(:default-modes ,default-modes)))))

@export
(defmethod ipc-buffer-delete ((buffer qt-buffer)))

@export
(defmethod ipc-buffer-load ((buffer qt-buffer) uri)
  (qt:web-engine-view-load (qt-object buffer) uri))

@export
(defmethod ipc-buffer-evaluate-javascript ((buffer qt-buffer) javascript &key callback)
  (declare (ignore callback))
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (qt-object buffer)) javascript))

@export
(defmethod ipc-minibuffer-evaluate-javascript ((window qt-window) javascript &key callback)
  (declare (ignore callback))
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (minibuffer-view window)) javascript))

@export
(defmethod ipc-buffer-enable-javascript ((buffer qt-buffer) value)
  (declare (ignore buffer))
  (declare (ignore value))
  (error 'unsupported-operation))

@export
(defmethod ipc-buffer-set-proxy ((buffer qt-buffer) &optional proxy-uri (ignore-hosts (list nil)))
  (declare (ignore buffer))
  (declare (ignore proxy-uri))
  (declare (ignore ignore-hosts))
  (error 'unsupported-operation))

@export
(defmethod ipc-generate-input-event ((window qt-window) event))

@export
(defmethod ipc-generated-input-event-p ((window qt-window) event))
