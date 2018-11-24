(in-package :port)

(defun set-conversion-table ()
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "
")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) ""))

(defun run-loop ())

(defun run-program ())
