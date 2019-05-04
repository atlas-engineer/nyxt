;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar *options* ()
  "The list of command line options.")
(defvar *free-args* ()
  "The list of positional command line arguments.")

(defvar *interface* nil
  "The CLOS object responsible for rendering the interface.")

;; TODO: Move commands with their hooks to a REMOTE-INTERFACE slot?
;; Better: make this part of modes, so that we can have mode-specific commands.
(defvar *available-hooks* (make-hash-table :test #'equalp)
  "A hash of all available hooks.")
(defvar *available-commands* (make-hash-table :test #'equalp)
  "A hash of all available commands.")

(defvar *swank-port* 4006
  "The port that swank will open a new server on (default Emacs slime port
  is 4005, default set to 4006 in Next to avoid collisions).")

;; TODO: Unused.  Remove?  Might be useful when we introspect classes.
(defvar *package-symbols* nil
  "The package symbols available, populated by helper function
  load-package-symbols.")

;; TODO: This is barely useful since we don't have any global.  It is used by
;; INSPECT-VARIABLE.  We need to augment the function so that we can inspect
;; *INTERFACE* and classes.
(defvar *package-globals* nil
  "The package global variables available, populated by helper
  function load package-globals")

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
