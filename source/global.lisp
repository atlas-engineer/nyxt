;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar *options* ()
  "The list of command line options.")
(defvar *free-args* ()
  "The list of positional command line arguments.")

(defvar *interface* nil
  "The CLOS object responsible for rendering the interface.")

(defvar *available-hooks* (make-hash-table :test #'equalp)
  "A hash of all available hooks.")
(defvar *available-commands* (make-hash-table :test #'equalp)
  "A hash of all available commands.")
(defvar *deferred-variables* ()
  "A list of functions which set globals which are deferred until startup for evaluation.")
(defvar *deferred-mode-initializations* ()
  "A list of functions invoked on start for modes.")
(defvar *global-map* (make-hash-table :test 'equal)
  "A global key map, available in every mode/buffer.")
(defvar *swank-port* 4006
  "The port that swank will open a new server on (default Emacs slime port
  is 4005, default set to 4006 in Next to avoid collisions).")
(defvar *package-symbols* nil
  "The package symbols available, populated by helper function
  load-package-symbols.")
(defvar *package-globals* nil
  "The package global variables available, populated by helper
  function load package-globals")
(defvar *platform-port-poll-interval* 0.015
  "The speed at which to poll the XML-RPC endpoint of a platform-port
  to see if it is ready to begin accepting XML-RPC commands.")

(deferredvar *init-file-path* (xdg-config-home "init.lisp")
  "The path where the system will look to load an init file from.")
(deferredvar *history-db-path* (xdg-data-home "history.db")
  "The path where the system will create/save the history database.")
(deferredvar *bookmark-db-path* (xdg-data-home "bookmark.db")
  "The path where the system will create/save the bookmark database.")
(deferredvar *cookies-path* (xdg-data-home "cookies.txt")
  "The path for cookies in the GTK Version of Next")

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
