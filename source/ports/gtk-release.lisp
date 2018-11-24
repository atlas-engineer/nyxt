(in-package :port)

(defun run-loop ()
  #+sbcl(loop (sb-sys:serve-all-events)))

(defun run-program ()
  ;; TODO: Make path configurable from the build system.
  (let* ((path  "next-gtk-webkit"))
    (bt:make-thread
     (lambda () (uiop:run-program path)))))
