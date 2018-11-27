(in-package :next)

(defmethod set-conversion-table ((port port))
  (setf (gethash "SPACE" *character-conversion-table*) " ")
  (setf (gethash "BACKSPACE" *character-conversion-table*) "")
  (setf (gethash "RETURN" *character-conversion-table*) "
")
  (setf (gethash "HYPHEN" *character-conversion-table*) "-")
  (setf (gethash "ESCAPE" *character-conversion-table*) "")
  (setf (gethash "TAB" *character-conversion-table*) "	"))

(define-key *global-map* (key "S-t") 'make-visible-new-buffer)
(define-key *global-map* (key "S-n") 'make-window)
(define-key *global-map* (key "S-w") 'delete-window)
(define-key *global-map* (key "C-TAB") 'switch-buffer-next)
;; TODO: Switch buffer previous fix
(define-key *global-map* (key "C-s-TAB") 'switch-buffer-previous)
