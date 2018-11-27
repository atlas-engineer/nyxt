;;; class to abstractly handle differences between platform ports

(in-package :next)

(defvar *port* nil
  "The CLOS object responible for handling the platform port.")

(defclass port () ())

(defgeneric set-conversion-table (port))

(defgeneric run-program (port))

(defgeneric run-loop (port))

(defgeneric kill-program (port))
