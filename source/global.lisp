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

  (setf *interface* (make-instance *remote-interface-class*))

It's possible to run multiple interfaces of Next at the same time.  You can
let-bind *interface* to temporarily switch interface.")

(declaim (type next-hooks:hook-void *after-init-hook*))
@export
(defvar *after-init-hook* (make-instance 'next-hooks:hook-void)
  "The entry-point object to configure everything in Next.
The hook takes no argument.

This hook is run after the `*interface*' is instantiated and before the
`startup-function' is run.

Add a handler can be added with

  (next-hooks:add-hook *after-init-hook*
    (next-hooks:make-handler-void #'my-foo-function))")

@export
(defparameter *use-session* t
  "If nil, don't restore nor store the session.")

@export
(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Next to avoid collisions).")

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
