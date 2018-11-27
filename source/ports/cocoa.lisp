(in-package :next)

(defclass port ()
  ((running-thread :accessor running-thread)))

(defmethod set-conversion-table ((port port))
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "
")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) "")
  (setf (gethash "TAB" *character-conversion-table*) "	"))

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

(define-key *global-map* (key "S-t") 'make-visible-new-buffer)
(define-key *global-map* (key "S-n") 'make-window)
(define-key *global-map* (key "S-w") 'delete-window)
(define-key *global-map* (key "S-q") 'kill)
(define-key *global-map* (key "C-TAB") 'switch-buffer-next)
(define-key *global-map* (key "C-s-TAB") 'switch-buffer-previous)
