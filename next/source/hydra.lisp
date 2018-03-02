;;; Make multiple binding with a common prefix
(in-package :next)

(defmacro defhydra (name &body suffixes)
  "Define a hydra named 'name'"
  (let ((hydra-object (gensym))
	(completion-function (gensym)))
    `(progn
       (defclass ,hydra-object ()
	 ((key :accessor key :initarg :key)
	  (callback :accessor callback :initarg :callback)))
       
       (defmethod print-object ((,hydra-object ,hydra-object) stream)
	 (let ((docstring (documentation (callback ,hydra-object) 'function)))
	   (format stream "~A - ~A"
		   (key ,hydra-object)
		   (or docstring (callback ,hydra-object)))))
       
       (defun ,completion-function (input)
	 (let ((completions (fuzzy-match input (quote ,suffixes) #'car)))
	   (map 'list
		(lambda (arg)
		  (make-instance (quote ,hydra-object)
				 :key (first arg)
				 :callback (second arg)))
	        completions)))
       
       (defun ,name ()
	 (with-result (suffix (read-from-minibuffer
			       (mode *minibuffer*)
			       :completion #',completion-function))
	   (cond ,@(map 'list (lambda (arg)
				(list (list 'equal '(key suffix) (first arg))
				      (list (second arg))))
			suffixes)))))))
