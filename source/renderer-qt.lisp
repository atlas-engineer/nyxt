;;; renderer-qt.lisp --- functions for creating Qt interface onscreen

(in-package :next)

(defclass-export qt-browser (browser)
  ((application :accessor application)))

(setf *browser-class* 'qt-browser)

(defmethod ffi-initialize ((browser qt-browser) urls startup-timestamp)
  (log:debug "Initializing Qt Interface")
  (flet ((initialize ()
           (setf (application browser)
                 (qt:new-q-application 1 (cffi:foreign-alloc :string
                                                             :initial-contents (list "Next")
                                                             :null-terminated-p t)))
           (finalize browser urls startup-timestamp)
           (qt:application-exec (application browser))))
    #+sbcl
    (sb-int:with-float-traps-masked (:invalid :inexact :overflow)
      (log:debug "SBCL initialization")
      (initialize))
    #-sbcl
    (initialize)))

(defmethod ffi-kill-browser ((browser qt-browser))
  (qt:application-quit (application browser)))

(defclass-export qt-window (window)
  ((qt-object :accessor qt-object)
   (box-layout :accessor box-layout)
   (minibuffer-view :accessor minibuffer-view)))

(define-class-type window)
(declaim (type (window-type) *window-class*))
(export-always '*window-class*)
(defparameter *window-class* 'qt-window)

(defclass-export qt-buffer (buffer)
  ((qt-object :accessor qt-object)))

(define-class-type buffer)
(declaim (type (buffer-type) *buffer-class*))
(export-always '*buffer-class*)
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
    (qt:widget-install-key-press-filter
     (qt-object window)
     (lambda (event)
       (on-signal-key-press-event window event)))
    (qt:widget-show qt-object)))

(defmethod printable-p ((window qt-window) event)
  "Return the printable value of EVENT."
  (match (qt:key-string event)
    (" " "space")
    ("-" "hyphen")
    (_ (qt:key-string event))))

(defmethod on-signal-key-press-event ((sender qt-window) event)
  (when (qt:key-string event)
    (alex:appendf (key-stack *browser*)
                  (list (keymap:make-key :code (qt:key-code event)
                                         :value (string-downcase (qt:key-string event))
                                         :modifiers (qt:modifiers event)
                                         :status :pressed)))
    (funcall (input-dispatcher sender)
             event (active-buffer sender)
             sender (printable-p sender event))))

(defmethod on-signal-destroy ((window qt-window))
  (window-delete window))

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
  (qt:load-started-listener-connect
   (qt-object buffer)
   (lambda ()
     (on-signal-load-committed buffer (qt:web-engine-view-url (qt-object buffer)))))
  (qt:load-finished-listener-connect
   (qt-object buffer)
   (lambda ()
     (on-signal-load-finished buffer (qt:web-engine-view-url (qt-object buffer))))))

(defmethod ffi-window-make ((browser qt-browser))
  "Make a window."
  (make-instance *window-class*))

(defmethod ffi-window-to-foreground ((window qt-window))
  "Show window in foreground."
  (qt:window-present (qt-object window)))

(defmethod ffi-window-set-title ((window qt-window) title)
  "Set the title for a window."
  (qt:window-set-window-title (qt-object window) title)
  title)

(defmethod ffi-window-active ((browser qt-browser))
  "Return the window object for the currently active window."
  (setf (slot-value browser 'last-active-window)
        (or (find-if #'qt:window-is-active-window (window-list) :key #'qt-object)
            (slot-value browser 'last-active-window))))

(defmethod ffi-window-set-active-buffer ((window qt-window) (buffer qt-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER."
  (qt:widget-set-parent (qt-object (active-buffer window)) (cffi:null-pointer))
  (qt:layout-insert-widget (box-layout window) 0 (qt-object buffer))
  (qt:widget-show (qt-object buffer))
  (setf (active-buffer window) buffer))

(defmethod ffi-window-set-minibuffer-height ((window qt-window) height)
  (qt:widget-set-fixed-height (minibuffer-view window) height))

(defmethod ffi-buffer-make ((browser qt-browser) &key title default-modes)
  "Make buffer with title TITLE and modes DEFAULT-MODES."
  (apply #'make-instance *buffer-class*
         (append (when title `(:title ,title))
                 (when default-modes `(:default-modes ,default-modes)))))

(defmethod ffi-buffer-delete ((buffer qt-buffer)))

(defmethod ffi-buffer-load ((buffer qt-buffer) uri)
  (qt:web-engine-view-load (qt-object buffer) uri))

(defmethod ffi-buffer-evaluate-javascript ((buffer qt-buffer) javascript &key callback)
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (qt-object buffer)) javascript callback))

(defmethod ffi-minibuffer-evaluate-javascript ((window qt-window) javascript &key callback)
  (qt:web-engine-page-run-javascript (qt:web-engine-view-page (minibuffer-view window)) javascript callback))

(defmethod ffi-generate-input-event ((window qt-window) event))

(defmethod ffi-generated-input-event-p ((window qt-window) event))

(defmethod ffi-within-renderer-thread ((browser qt-browser) thunk)
  (declare (ignore browser))
  ;; TODO: Test this!
  (funcall thunk))
