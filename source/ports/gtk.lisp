(in-package :next)

(defmethod set-conversion-table ((port port))
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "DELETE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) ""))
