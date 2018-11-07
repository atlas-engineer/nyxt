;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar *available-hooks* (make-hash-table :test #'equalp)
  "A hash of all available hooks.")
(defvar *available-commands* (make-hash-table :test #'equalp)
  "A hash of all available commands")
(defvar *deferred-variables* ()
  "A list of functions which set globals which are deferred until startup for evaluation.")
(defvar *global-map* (make-hash-table :test 'equalp)
  "A global key map, available in every mode/buffer.")
(defvar *active-buffer* ()
  "The currently active buffer, do not modify this variable directly.")
(defvar *minibuffer* nil
  "A variable to store the minibuffer.")
(defvar *buffers* ()
  "A list of all existing buffers.")
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
(defvar *start-page-url* "http://next.atlas.engineer/start"
  "The url of the first buffer opened by Next when started.")
(defvar *default-new-buffer-url* "about:blank"
  "The url set to a new blank buffer opened by Next.")
(defvar *package-symbols* nil
  "The package symbols available, populated by helper function
  load-package-symbols.")
(defvar *package-globals* nil
  "The package global variables available, populated by helper
  function load package-globals")
(defvar *current-completions* ()
  "A global variable used to store current completions for a
  completion function that has a static list.")
(defvar *interface* nil
  "The CLOS object responsible for rendering the interface.")
(defvar *minibuffer-closed-height* 10
  "The height of the minibuffer when closed.")
(defvar *minibuffer-open-height* 300
  "The height of the minibuffer when open.")
(deferredvar *init-file-path* (xdg-config-home "init.lisp")
  "The path where the system will look to load an init file from.")
(deferredvar *history-db-path* (xdg-data-home "history.db")
  "The path where the system will create/save the history database.")
(deferredvar *bookmark-db-path* (xdg-data-home "bookmark.db")
  "The path where the system will create/save the bookmark database.")
(deferredvar *cookie-path-dir* (xdg-data-home)
  "The path for cookies in the GTK Version of Next")
