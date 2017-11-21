;;;; link-hint.lisp --- functions to enable link hinting and navigation

(in-package :next)

(defun add-span ()
  (defparen add-span (let ((element (ps:chain document (create-element "span"))))
			 (setf (ps:@ element text-content) "some-text")
			 (ps:chain document body (append-child element))
			 "some return"))
  (interface:web-view-execute (view *active-buffer*) add-span))
