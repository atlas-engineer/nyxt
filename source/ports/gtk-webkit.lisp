(in-package :next)

(defmethod set-conversion-table ((port port))
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "DELETE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) ""))

(defmethod run-loop ((port port))
  #+sbcl(loop (sb-sys:serve-all-events)))

(defmethod run-program ((port port))
  ;; TODO: Make path configurable from the build system.
  (let* ((path  "next-gtk-webkit"))
    (bt:make-thread
     (lambda () (uiop:run-program path)))))
