;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar *options* ()
  "The list of command line options.")
(defvar *free-args* ()
  "The list of positional command line arguments.")

(defvar *interface* nil
  "The entry-point object to a complete instance of Next.
It can be initialized with

  (setf *interface* (make-instance 'remote-interface))

Much of Next code assumes a valid *interface*.  This could change in the future
when multiple interfaces are supported.")

(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Next to avoid collisions).")

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
