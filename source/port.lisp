;;; class to abstractly handle differences between platform ports

(in-package :next)

;; TODO: Function-values for slots are not a good idea: should the value change,
;; we have no way to access the old value.

(defclass port ()
  ((name :initarg :name :initform +platform-port-command+ :accessor name
         :documentation "Basename of the executable.")
   (path :initarg :path :initform #'derive-path-from-name
         :documentation "Full path to the executable.
It can also be a function that takes NAME as argument and returns the path as a
string.")
   (args :initarg :args :initform nil
         :documentation "List of strings passed as argument to the executable.")
   (log-file :initarg :log-file :initform #'derive-logfile-from-name
             :documentation "Log file for the platform port.
It can also be a function that takes NAME as argument and returns the log file
as a string.")
   (running-process :accessor running-process
                    :initform nil)))

(defun port-accessor (port slot &rest args)
  (if (functionp (slot-value port slot))
      (apply (slot-value port slot) args)
      (slot-value port slot)))

(defmethod path ((port port))
  (let ((p (port-accessor port 'path (name port))))
    (if (uiop:file-exists-p p)
        (uiop:truename* p)
        p)))

(defmethod args ((port port))
  (port-accessor port 'args))

(defmethod log-file ((port port))
  (port-accessor port 'log-file (name port)))

(defun derive-path-from-name (&rest names)
  "Find a platform port binary.

It is looked in the ports/ subfolder corresponding to the
NAME (e.g. \"gtk-webkit\") relative to the 'next' executable or the ASD system.

If it is not found, NAME is used as is and will be looked for in PATH.

This function is an acceptable value for the PATH slot of the PORT class."
  (let ((names (or names '("next-gtk-webkit" "next-pyqt-webengine")))
        (root-dir (uiop:pathname-directory-pathname
                   (or (let ((program (executable-find (uiop:argv0))))
                         (when program
                           (uiop:truename* program)))
                       (nth-value 2 (asdf:locate-system :next))))))
    (or
     (when root-dir
       (loop for name in names
             for dir-name = (str:replace-all "next-" "" (file-namestring name))
             for file-in-subdir = (merge-pathnames
                                   (file-namestring name)
                                   (merge-pathnames (format nil "ports/~a/" dir-name)
                                                    root-dir))
             when (uiop:file-exists-p file-in-subdir)
               return file-in-subdir))
     ;; As a last resort, return the first argument unchanged.
     (first names))))

(defun derive-logfile-from-name (name)
  "This is an acceptable value for the LOG-FILE slot of the PORT class."
  (xdg-data-home (str:concat (file-namestring name) ".log")))

(defmethod run-loop ((port port))
  (if (running-process port)
      (uiop:wait-process (running-process port))
      (log:error "Platform port was started externally, kill the process and try again.")))

(defmethod run-program ((port port))
  (let ((port-path (path port))
        (port-args (args port))
        (port-log-file (log-file port)))
    (log:info "Current directory: ~a" *default-pathname-defaults*)
    (log:info "Platform port path: ~a" port-path)
    (log:info "Platform port arguments: ~a" port-args)
    (log:info "Platform port log file: ~a" port-log-file)
    (ensure-directories-exist (directory-namestring port-log-file))
    (setf (running-process port)
          (uiop:launch-program (cons port-path port-args)
                               :output port-log-file
                               :error-output :output))))

(defmethod kill-port ((port port))
  (if (uiop:process-alive-p (running-process port))
      (kill-program (uiop:process-info-pid (running-process port)))
      (log:error "Process ~a is not running" (running-process port))))
