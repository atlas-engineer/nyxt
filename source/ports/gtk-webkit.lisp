(in-package :next)

(defvar *gtk-webkit-command-name* "next-gtk-webkit"
  "Name of the GTK-Webkit platform port executable.")

(defun gtk-webkit-command-path ()
  (or
   (probe-file (merge-pathnames *gtk-webkit-command-name*))
   (probe-file (merge-pathnames *gtk-webkit-command-name*
                                     (merge-pathnames
                                      "ports/gtk-webkit/")))
   *gtk-webkit-command-name*))

(defvar *gtk-webkit-command* #'gtk-webkit-command-path
  "Path to the GTK-Webkit platform port executable.
This can be either a string or a function of zero argument returning a string.")

(defun gtk-webkit-list-args ()
  "Derive platform port arguments dynamically at runtime.
This is useful if, for instance, the *CORE-PORT* gets changed after startup."
  (unless (find-port:port-open-p (getf *platform-port-socket* :port))
    (let ((new-port (find-port:find-port)))
      (format *error-output* "Platform port socket ~a seems busy, trying ~a instead.~%"
              (getf *platform-port-socket* :port) new-port)
      (setf (getf *platform-port-socket* :port) new-port)))
  (list "--port" (write-to-string (getf *platform-port-socket* :port))
        "--core-socket" (format nil "http://localhost:~a/RPC2" *core-port*)))

(defvar *gtk-webkit-args* #'gtk-webkit-list-args
  "Arguments to pass to the GTK-Webkit platform port executable.
This can be either a string or a function of zero argument returning a string.")

(defvar *gtk-webkit-log* #P"next-gtk-webkit.log"
  "Where to store the log of the GTK-Webkit platform port.")

(defclass port ()
  ((running-process :accessor running-process)))

(defmethod set-conversion-table ((port port))
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "DELETE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) ""))

(defmethod run-loop ((port port))
  (uiop:wait-process (running-process port)))

(defmethod run-program ((port port))
  (let ((user-temp-directory (merge-pathnames
                              (format nil "next-~a/" (uiop:getenv "USER"))
                              *temp-directory*))
        (port-path (if (functionp *gtk-webkit-command*)
                       (funcall *gtk-webkit-command*)
                       *gtk-webkit-command*))
        (port-args (if (functionp *gtk-webkit-args*)
                       (funcall *gtk-webkit-args*)
                       *gtk-webkit-args*)))
    ;; WARNING: :mode is an SBCL / CMUCL extension.
    (ensure-directories-exist user-temp-directory :mode #o700)
    (format t "Current directory: ~a~%" *default-pathname-defaults*)
    (format t "Platform port path: ~a~%" port-path)
    (format t "Platform port arguments: ~a~%" port-args)
    (setf (running-process port)
          (uiop:launch-program (cons port-path port-args)
                               :output (merge-pathnames *gtk-webkit-log* user-temp-directory)
                               :error-output :output))))

(defmethod kill-program ((port port))
  (uiop:run-program
   (list "kill" "-15"
         (write-to-string
          (uiop/launch-program:process-info-pid
           (running-process port))))))
