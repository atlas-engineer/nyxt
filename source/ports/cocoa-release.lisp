(in-package :next)

(defmethod run-loop ((port port))
  #+sbcl(loop (sb-sys:serve-all-events)))

(defmethod run-program ((port port))
  (let* ((path (uiop/image:argv0))
                (path (cl-strings:shorten path (- (length path) 4) :truncate-string ""))
         (path (concatenate 'string path "cocoa-webkit")))
    (setf (running-thread port)
          (bt:make-thread
           (lambda () (uiop:run-program path))))))

(defmethod kill-program ((port port))
  (bt:destroy-thread (running-thread port)))
