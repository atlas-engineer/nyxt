;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar *options* ()
  "The list of command line options.")
(defvar *free-args* ()
  "The list of positional command line arguments.")

(defvar *core-port* 8081
  "The XML-RPC server port of the Lisp core.")
(defvar *platform-port-socket* '(:host "localhost" :port 8082)
  "The XML-RPC remote socket of the platform-port.")

(defvar *minibuffer* nil
  "A variable to store the minibuffer.")
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
(defvar *character-conversion-table* (make-hash-table :test 'equalp)
  "A table used to convert between special characters across different operating systems.")
(defvar *scroll-distance* 50
  "The distance scroll-down or scroll-up will scroll.")
(defvar *horizontal-scroll-distance* 50
  "Horizontal scroll distance. The distance scroll-left or scroll-right
  will scroll.")
(defvar *current-zoom-ratio* 1.0
  "The current zoom relative to the default zoom.")
(defvar *zoom-ratio-step* 0.2
  "The step size for zooming in and out.")
(defvar *zoom-ratio-min* 0.2
  "The minimum zoom ratio relative to the default.")
(defvar *zoom-ratio-max* 5.0
  "The maximum zoom ratio relative to the default.")
(defvar *zoom-ratio-default* 1.0
  "The default zoom ratio.")
(defvar *swank-port* 4006
  "The port that swank will open a new server on (default Emacs slime port
  is 4005, default set to 4006 in Next to avoid collisions).")
(defvar *start-page-url* "https://next.atlas.engineer/quickstart"
  "The URL of the first buffer opened by Next when started.")
(defvar *default-new-buffer-url* "https://next.atlas.engineer/start"
  "The URL set to a new blank buffer opened by Next.")
(defvar *default-new-buffer-mode* nil
  "The mode a buffer will open in by default")
(defvar *package-symbols* nil
  "The package symbols available, populated by helper function
  load-package-symbols.")
(defvar *package-globals* nil
  "The package global variables available, populated by helper
  function load package-globals")
(defvar *minibuffer-closed-height* 0
  "The height of the minibuffer when closed.")
(defvar *minibuffer-open-height* 200
  "The height of the minibuffer when open.")
(defvar *minibuffer-echo-height* 25
  "The height of the minibuffer when echoing.")
(defvar *platform-port-poll-interval* 0.015
  "The speed at which to poll the XML-RPC endpoint of a platform-port
  to see if it is ready to begin accepting XML-RPC commands.")
(deferredvar *init-file-path* (xdg-config-home "init.lisp")
  "The path where the system will look to load an init file from.")
(deferredvar *history-db-path* (xdg-data-home "history.db")
  "The path where the system will create/save the history database.")
(deferredvar *bookmark-db-path* (xdg-data-home "bookmark.db")
  "The path where the system will create/save the bookmark database.")
(deferredvar *cookie-path-dir* (xdg-data-home)
  "The path for cookies in the GTK Version of Next")
