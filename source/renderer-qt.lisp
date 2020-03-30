;;; renderer-qt.lisp --- functions for creating Qt interface onscreen

(in-package :next)

(define-condition unsupported-operation (error)
  ()
  (:report (lambda (c stream)
             (declare (ignore c))
             (format stream "~a" "Unsupported operation for this renderer."))))

(defclass-export qt-browser (browser)
  ((application :accessor application)))

(define-class-type browser)
(declaim (type (browser-type) *browser-class*))
(serapeum:export-always '*browser-class*)
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

(defclass-export qt-window (window)
  ((qt-object :accessor qt-object)
   (box-layout :accessor box-layout)
   (minibuffer-view :accessor minibuffer-view)))

(define-class-type window)
(declaim (type (window-type) *window-class*))
(serapeum:export-always '*window-class*)
(defparameter *window-class* 'qt-window)

(defclass-export qt-buffer (buffer)
  ((qt-object :accessor qt-object)))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
(serapeum:export-always '*buffer-class*)
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
    (ffi-window-set-minibuffer-height window (status-buffer-height window))
    (qt:widget-resize qt-object 1024 768)
    (qt:widget-show qt-object)))

(defmethod process-destroy ((window qt-window))
  (window-delete window))

(serapeum:export-always 'ffi-window-delete)
(defmethod ffi-window-delete ((window qt-window))
  "Delete a window object and remove it from the hash of windows.")

(defmethod ffi-window-fullscreen ((window qt-window))
  (qt:window-show-full-screen (qt-object window)))

(defmethod ffi-window-unfullscreen ((window qt-window))
  (qt:window-show-normal (qt-object window)))

(defmethod initialize-instance :after ((buffer qt-buffer) &key)
  (hooks:run-hook (buffer-before-make-hook *browser*) buffer)
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

(serapeum:export-always 'ffi-window-make)
(defmethod ffi-window-make ((browser qt-browser))
  "Make a window."
  (make-instance *window-class*))

(serapeum:export-always 'ffi-window-to-foreground)
(defmethod ffi-window-to-foreground ((window qt-window))
  "Show window in foreground."
  (qt:window-present (qt-object window)))

(serapeum:export-always 'ffi-window-set-title)
(defmethod ffi-window-set-title ((window qt-window) title)
  "Set the title for a window."
  (qt:window-set-window-title (qt-object window) title)
  title)

(serapeum:export-always 'ffi-window-active)
(defmethod ffi-window-active ((browser qt-browser))
  "Return the window object for the currently active window."
  (setf (slot-value browser 'last-active-window)
        (or (find-if #'qt:window-is-active-window (window-list) :key #'qt-object)
            (slot-value browser 'last-active-window))))

(serapeum:export-always 'ffi-window-set-active-buffer)
(defmethod ffi-window-set-active-buffer ((window qt-window) (buffer qt-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (qt:widget-set-parent (qt-object (active-buffer window)) (cffi:null-pointer))
  (qt:layout-insert-widget (box-layout window) 0 (qt-object buffer))
  (qt:widget-show (qt-object buffer))
  (setf (active-buffer window) buffer))

(serapeum:export-always 'ffi-window-set-minibuffer-height)
(defmethod ffi-window-set-minibuffer-height ((window qt-window) height)
  (qt:widget-set-fixed-height (minibuffer-view window) height))

(serapeum:export-always 'ffi-buffer-make)
(defmethod ffi-buffer-make ((browser qt-browser) &key title default-modes)
  "Make buffer with title TITLE and modes DEFAULT-MODES."
  (apply #'make-instance *buffer-class*
         (append (when title `(:title ,title))
                 (when default-modes `(:default-modes ,default-modes)))))

(serapeum:export-always 'ffi-buffer-delete)
(defmethod ffi-buffer-delete ((buffer qt-buffer)))

(serapeum:export-always 'ffi-buffer-load)
(defmethod ffi-buffer-load ((buffer qt-buffer) uri)
  (qt:web-engine-view-load (qt-object buffer) uri))

(serapeum:export-always 'ffi-buffer-evaluate-javascript)
(defmethod ffi-buffer-evaluate-javascript ((buffer qt-buffer) javascript &key callback)
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (qt-object buffer)) javascript callback))

(serapeum:export-always 'ffi-minibuffer-evaluate-javascript)
(defmethod ffi-minibuffer-evaluate-javascript ((window qt-window) javascript &key callback)
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (minibuffer-view window)) javascript callback))

(serapeum:export-always 'ffi-buffer-enable-javascript)
(defmethod ffi-buffer-enable-javascript ((buffer qt-buffer) value)
  (declare (ignore buffer))
  (declare (ignore value))
  (error 'unsupported-operation))

(serapeum:export-always 'ffi-buffer-set-proxy)
(defmethod ffi-buffer-set-proxy ((buffer qt-buffer) &optional proxy-uri (ignore-hosts (list nil)))
  (declare (ignore buffer))
  (declare (ignore proxy-uri))
  (declare (ignore ignore-hosts))
  (error 'unsupported-operation))

(serapeum:export-always 'ffi-generate-input-event)
(defmethod ffi-generate-input-event ((window qt-window) event))

(serapeum:export-always 'ffi-generated-input-event-p)
(defmethod ffi-generated-input-event-p ((window qt-window) event))
