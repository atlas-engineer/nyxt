(defun run-loop ()
  (swank::simple-repl)
  #+sbcl(loop (sb-sys:serve-all-events)))
