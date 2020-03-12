;;; renderer-qt.lisp --- functions for creating Qt interface onscreen

(in-package :next)
(annot:enable-annot-syntax)

@export
@export-accessors
(defclass qt-browser (browser) ())

(define-class-type browser)
(declaim (type (browser-type) *browser-class*))
@export
(defparameter *browser-class* 'qt-browser)

(defmethod initialize ((browser qt-browser) urls startup-timestamp)
  (log:debug "Initializing Qt Interface"))

(defmethod kill-interface ((browser qt-browser)))

@export
@export-accessors
(defclass qt-window (window)
  ((qt-object :accessor qt-object)))

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

(defmethod initialize-instance :after ((window qt-window) &key))

(defmethod process-destroy ((window qt-window))
  (window-delete window))

@export
(defmethod ipc-window-delete ((window qt-window))
  "Delete a window object and remove it from the hash of windows.")

(defmethod ipc-window-fullscreen ((window qt-window)))

(defmethod ipc-window-unfullscreen ((window qt-window)))

(defmethod initialize-instance :after ((buffer qt-buffer) &key))

@export
(defmethod ipc-window-make ((browser qt-browser))
  "Make a window."
  (make-instance *window-class*))

@export
(defmethod ipc-window-to-foreground ((window qt-window))
  "Show window in foreground.")

@export
(defmethod ipc-window-set-title ((window qt-window) title)
  "Set the title for a window.")

@export
(defmethod ipc-window-active ((browser qt-browser))
  "Return the window object for the currently active window.")

@export
(defmethod ipc-window-set-active-buffer ((window qt-window) (buffer qt-buffer))
  "Set BROWSER's WINDOW buffer to BUFFER. "
  buffer)

@export
(defmethod ipc-window-set-minibuffer-height ((window qt-window) height))

@export
(defmethod ipc-buffer-make ((browser qt-browser) &key title default-modes)
  "Make buffer with title TITLE and modes DEFAULT-MODES."
  (apply #'make-instance *buffer-class*
         (append (when title `(:title ,title))
                 (when default-modes `(:default-modes ,default-modes)))))

@export
(defmethod ipc-buffer-delete ((buffer qt-buffer)))

@export
(defmethod ipc-buffer-load ((buffer qt-buffer) uri))

@export
(defmethod ipc-buffer-evaluate-javascript ((buffer qt-buffer) javascript &key callback))

@export
(defmethod ipc-minibuffer-evaluate-javascript ((window qt-window) javascript &key callback))

@export
(defmethod ipc-buffer-enable-javascript ((buffer qt-buffer) value))

@export
(defmethod ipc-buffer-set-proxy ((buffer qt-buffer) &optional proxy-uri (ignore-hosts (list nil))))

@export
(defmethod ipc-generate-input-event ((window qt-window) event))

@export
(defmethod ipc-generated-input-event-p ((window qt-window) event))
