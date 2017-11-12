;;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar global-map (make-hash-table :test 'equalp)
  "A global key map, available in every mode/buffer")
(defvar *active-buffer* ()
  "The currently active buffer")
(defvar *minibuffer* nil
  "A variable to store the mini-buffer")
(defvar *buffers* ()
  "A list of all existing buffers")
(defvar *scroll-distance* 30
  "The distance scroll-down or scroll-up will scroll.")
