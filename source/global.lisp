;;; global.lisp --- global variable and parameter declarations
;; Packagers are welcome to customize the `defparameter's to suit the host system.

(in-package :next)

(defvar *options* '()
  "The list of command line options.")
(defvar *free-args* '()
  "The list of positional command line arguments.")

(defvar *keep-alive* t
  "If non-nil, don't terminate the Lisp process when quitting the browser.
This is useful when the browser is run from a REPL so that quitting does not
close the connection.")

(serapeum:export-always '*browser*)
(defvar *browser* nil
  "The entry-point object to a complete instance of Next.
It can be initialized with

  (setf *browser* (make-instance *browser-class*))

It's possible to run multiple interfaces of Next at the same time.  You can
let-bind *browser* to temporarily switch interface.")

(declaim (type hooks:hook-void *after-init-hook*))
(serapeum:export-always '*after-init-hook*)
(defvar *after-init-hook* (make-instance 'hooks:hook-void)
  "The entry-point object to configure everything in Next.
The hook takes no argument.

This hook is run after the `*browser*' is instantiated and before the
`startup-function' is run.

Add a handler can be added with

  (hooks:add-hook *after-init-hook*
    (hooks:make-handler-void #'my-foo-function))")

(serapeum:export-always '*session*)
;; We don't set *session* to (xdg-data-home "sessions/default.lisp") because it
;; would result in XDG_DATA_HOME being expanded at compile-time.
(defparameter *session* "default"
  "Original value is the default session name.
Current value is the current session file.
See `derive-session'.")

(serapeum:export-always '*swank-port*)
(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Next to avoid collisions).")

(serapeum:export-always '+version+)
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
