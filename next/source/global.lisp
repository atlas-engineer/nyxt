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
(defvar *scroll-distance* 15
  "The distance scroll-down or scroll-up will scroll.")
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
