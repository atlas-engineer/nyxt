(in-package "CCL")

(defclass next-application (ns:ns-application) ()
  (:metaclass ns:+ns-application))

(objc:defmethod (#/sendEvent: :void) ((self next-application) event)
  (if (eql #$NSKeyDown (#/type event))
      (unless (interface:process-event event)
	(call-next-method event))
      (call-next-method event)))

(defclass cocoa-ui-object (ui-object) ())

(defclass cocoa-application (application)
  ((standalone-p :initform nil :accessor cocoa-application-standalone-p)))

(defparameter *cocoa-application*
  (make-instance 'cocoa-application
		 :ui-object (make-instance 'cocoa-ui-object)))

(defmethod application-error ((self cocoa-application) condition error-pointer)
  (break-loop-handle-error condition error-pointer))

(defmethod parse-application-arguments ((self cocoa-application))
  (values nil nil nil nil))

(defmethod toplevel-function ((self cocoa-application) init-file)
  (declare (ignore init-file))
  (setf (cocoa-application-standalone-p self) t)
  (prepare-cocoa-application self)
  (start-cocoa-application self))

(defmethod prepare-cocoa-application ((self cocoa-application))
  (call-in-initial-process #'(lambda ()
			       (setq *nsapp* (load-cocoa-application self))
			       (next:start))))

(defmethod load-cocoa-application ((a cocoa-application))
  (with-autorelease-pool
    (let* ((bundle (#/mainBundle ns:ns-bundle))
	   (info (#/infoDictionary bundle))
	   (progname (#/objectForKey: info #@"CFBundleName")))
      (unless (%null-ptr-p progname)
	(#/setProcessName: (#/processInfo ns:ns-process-info) progname))
      (let* ((appclass next-application)
	     (app (#/sharedApplication appclass)))
	(when (%null-ptr-p app)
	  (error "Could not create shared instance of ~s"
		 (%get-cfstring classname)))
	app))))

(defun become-foreground-application ()
  (rlet ((psn #>ProcessSerialNumber))
    (#_GetCurrentProcess psn)
    (#_TransformProcessType psn #$kProcessTransformToForegroundApplication)))

(defun event-loop ()
  (loop
    (with-simple-restart (abort "Process the next event")
      (#/run *nsapp*))))

(defun run-event-loop ()
  (%set-toplevel nil)
  (assert (eq *current-process* *initial-process*))
  (change-class *initial-process* 'cocoa-event-process)
  (setf (process-name *initial-process*) "Cocoa event loop")
  (with-process-whostate ("Active")
    (event-loop)))

;; After the lisp starts up, it creates a listener thread.  The
;; initial thead then goes to sleep, waking up about 3 times a second
;; to do some housekeeping tasks.
;;
;; Because most of Cocoa needs to run on the initial thread, we
;; interrupt the initial thread, and use %set-toplevel and toplevel
;; (which are basically process-preset and process-reset for the
;; initial process) to make it start running the Cocoa event loop.  We
;; create a new thread to do the housekeeping.

(defmethod start-cocoa-application ((a cocoa-application))
  (flet ((startup ()
           (with-standard-initial-bindings
             (process-run-function "housekeeping" #'housekeeping-loop)
             (become-foreground-application)
	     (run-event-loop))))
    (process-interrupt *initial-process*
                       #'(lambda ()
                           (%set-toplevel #'startup)
                           (toplevel)))))
