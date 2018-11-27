(in-package :next)

(defmethod run-loop ((port port))
  #+sbcl(loop (sb-sys:serve-all-events)))

(defmethod run-program ((port port))
  ;; TODO: Make path configurable from the build system.
  (let* ((path  "next-gtk-webkit"))
    (bt:make-thread
     (lambda () (uiop:run-program path)))))
