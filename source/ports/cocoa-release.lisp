(in-package :port)

(defun run-loop ()
  #+sbcl(loop (sb-sys:serve-all-events)))

(defun run-program ()
  (let* ((path (uiop/image:argv0))
                (path (cl-strings:shorten path (- (length path) 4) :truncate-string ""))
         (path (concatenate 'string path "cocoa-webkit")))
    (bt:make-thread
     (lambda () (uiop:run-program path)))))
