;;; utility.lisp --- utility classes and functions

(in-package :next)

;; data node used to represent tree history
(defstruct node
  parent
  children
  data)

(defun load-file (input)
  (load input :if-does-not-exist nil))

(defun reload-init ()
  (load-file *init-file-path*))

(defun start-swank ()
  (ccl::call-in-event-process
   #'(lambda ()
       (swank:create-server :port *swank-port* :style :spawn :dont-close t))))
