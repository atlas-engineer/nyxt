(in-package :port)

(defun run-loop ()
  #+sbcl(loop (sb-sys:serve-all-events)))
