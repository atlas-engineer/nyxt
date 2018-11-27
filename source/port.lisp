;;; class to abstractly handle differences between platform ports

(in-package :next)

(defvar *port* nil
  "The CLOS object responible for handling the platform port.")

(defclass port () ())

(defgeneric set-conversion-table (port))

(defgeneric run-program (port))

(defgeneric run-loop (port))

(defgeneric kill-program (port))

(defmethod set-conversion-table ((port port))
  (declare (ignore port)))

(defmethod run-program ((port port))
  (declare (ignore port)))

(defmethod run-loop ((port port))
  (declare (ignore port)))

(defmethod kill-program ((port port))
  (declare (ignore port)))
