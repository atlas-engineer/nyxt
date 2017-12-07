;;;; help.lisp --- functions for the user to get help on nEXT

(in-package :next)

(defun package-symbols (p)
  (let (l) (do-symbols (s p l)
	     (push s l))))

(defun load-package-symbols ()
  (setf *package-symbols* (package-symbols :next)))

(defun load-package-globals ()
  (setf *package-globals* (filter-globals (package-symbols :next))))

(defun filter-globals (symbol-list)
  (remove-if-not
   (lambda (symbol)
     (cl-string-match:match-re "[*]+[a-zA-Z\-]+[*]" (symbol-name symbol)))
   symbol-list))

(defun variable-complete (input)
  (fuzzy-match input *package-globals* #'symbol-name))

(defun variable-inspect (input)
  (print (documentation input 'variable)))
