;;; global.lisp --- global variable and parameter declarations
;; Packagers are welcome to customize the `defparameter's to suit the host system.

(in-package :next)
(annot:enable-annot-syntax)

(defvar *options* '()
  "The list of command line options.")
(defvar *free-args* '()
  "The list of positional command line arguments.")

@export
(defvar *interface* nil
  "The entry-point object to a complete instance of Next.
It can be initialized with

  (setf *interface* (make-instance 'remote-interface))

Much of Next code assumes a valid *interface*.  This could change in the future
when multiple interfaces are supported.")

(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Next to avoid collisions).")

(defparameter +platform-port-name+ "engineer.atlas.next.platform")
(defparameter +platform-port-object-path+ "/engineer/atlas/next/platform")
(defparameter +platform-port-interface+ +platform-port-name+)

(defparameter +core-name+ "engineer.atlas.next.core")
(defparameter +core-object-path+ "/engineer/atlas/next/core")
(defparameter +core-interface+ +core-name+)

(defparameter +dbus-launch-command+ '("dbus-launch")
  "The command to start dbus, if necessary.
The first string is the command, the other strings are the arguments.")

@export
(defparameter +version+
  (let ((version (asdf/component:component-version (asdf:find-system :next)))
        (directory (asdf:system-source-directory :next)))
    (or (ignore-errors
         (uiop:with-current-directory (directory)
           (multiple-value-bind (current-commit)
               (uiop:run-program (list "git" "describe" "--always")
                                 :output '(:string :stripped t))
             (multiple-value-bind (tag-commit)
                 (uiop:run-program (list "git" "describe" version "--always")
                                   :output '(:string :stripped t))
               (concatenate 'string
                            version
                            (when (string/= tag-commit current-commit)
                              (format nil "-~a" current-commit)))))))
        version)))
