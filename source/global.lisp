;;; global.lisp --- global variable and parameter declarations
;; Packagers are welcome to customize the `defparameter's to suit the host system.

(in-package :next)

(defvar *renderer-class* 'gtk
  "The renderer class used when constructing the define-configuration
  macro.")

(defvar *options* '()
  "The list of command line options.")

(defvar *keep-alive* t
  "If non-nil, don't terminate the Lisp process when quitting the browser.
This is useful when the browser is run from a REPL so that quitting does not
close the connection.")

(export-always '*browser*)
(defvar *browser* nil
  "The entry-point object to a complete instance of Next.
It can be initialized with

  (setf *browser* (make-instance *browser-class*))

It's possible to run multiple interfaces of Next at the same time.  You can
let-bind *browser* to temporarily switch interface.")

(export-always '*init-file-path*)
(defvar *init-file-path* (make-instance 'data-path :basename "init")
  "The path of the initialization file.")

(export-always '*socket-path*)
(defvar *socket-path* (make-instance 'data-path :basename "next.socket")
  "Path string of the Unix socket used to communicate between different
instances of Next.

This path cannot be set from the init file because we want to be able to set and
use the socket without parsing any init file.")

(declaim (type hooks:hook-void *after-init-hook*))
(export-always '*after-init-hook*)
(defvar *after-init-hook* (make-instance 'hooks:hook-void)
  "The entry-point object to configure everything in Next.
The hook takes no argument.

This hook is run after the `*browser*' is instantiated and before the
`startup-function' is run.

Add a handler can be added with

  (hooks:add-hook *after-init-hook*
    (hooks:make-handler-void #'my-foo-function))")

(export-always '*swank-port*)
(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Next to avoid collisions).")

(export-always '+version+)
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
