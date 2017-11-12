;;;; global.lisp --- global variable and parameter declarations

(in-package :next)

(defvar *minibuffer-completion-function* nil
  "A variable to store the function used to generate completion candidates")
(defvar *minibuffer-callback* nil
  "A variable to store the function upon completion of the minibuffer read")
(defvar *minibuffer-callback-buffer* nil
  "A variable to store the buffer which originally requested the minibuffer read")
(defvar *minibuffer* nil
  "A variable to store the mini-buffer")
(defvar *buffers* ()
  "A list of all existing buffers")
(defvar *active-buffer* ()
  "The currently active buffer")
