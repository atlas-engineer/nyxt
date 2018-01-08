;;;; global.lisp --- global variable and parameter declarations

(in-package :next)

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
  is 4005, default set to 4006 in nEXT to avoid collisions).")
(defvar *start-page-url* "https://next-browser.github.io/start.html"
  "The url of the first buffer opened by nEXT when started.")
(defvar *package-symbols* nil
  "The package symbols available, populated by helper function
  load-package-symbols.")
(defvar *package-globals* nil
  "The package global variables available, populated by helper
  function load package-globals")
(defvar *init-file-path* "~/.next.d/init.lisp"
  "The path where the system will look to load an init file from.")
(defvar *history-db-path* "~/.next.d/history.db"
  "The path where the system will create/save the history database.")
(defvar *bookmark-db-path* "~/.next.d/bookmark.db"
  "The path where the system will create/save the bookmark database.")
(defvar *current-completions* ()
  "A global variable used to store current completions for a
  completion function that has a static list.")
(defvar *cookie-path-dir* "~/.next.d/"
  "The path for cookies in the GTK Version of nEXT")
