(in-package :next)

(defclass port ()
  ((running-process :accessor running-process)
   (running :accessor running :initform t)))

(defmethod set-conversion-table ((port port))
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "
")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) "")
  (setf (gethash "TAB" *character-conversion-table*) "	"))

(defmethod run-loop ((port port))
  (uiop:wait-process (running-process port)))

(defmethod run-program ((port port))
  (let* ((path (uiop/image:argv0))
                (path (cl-strings:shorten path (- (length path) 4) :truncate-string ""))
         (path (concatenate 'string path "cocoa-webkit")))
    (setf (running-process port) (uiop:launch-program path))))

(defmethod kill-program ((port port))
  (uiop:run-program
   (list "kill" "-15"
         (write-to-string
          (uiop/launch-program:process-info-pid
           (running-process port)))))
  (setf (running port) nil))

(define-key *global-map* (key "S-t") 'make-visible-new-buffer)
(define-key *global-map* (key "S-n") 'make-window)
(define-key *global-map* (key "S-w") 'delete-window)
(define-key *global-map* (key "S-q") 'kill)
(define-key *global-map* (key "C-TAB") 'switch-buffer-next)
(define-key *global-map* (key "C-s-TAB") 'switch-buffer-previous)
