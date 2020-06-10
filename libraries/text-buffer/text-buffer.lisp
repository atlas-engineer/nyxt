(in-package :text-buffer)

(defclass text-buffer (cluffer-simple-line:line) ())

(defclass cursor (cluffer-simple-line::right-sticky-cursor) ())

(defmethod move-forward-word ((cursor cursor))
  (print "moving forwards by a word"))

(defmethod move-backward-word ((cursor cursor))
  (print "moving backwards by a word"))

(defmethod string-representation ((buffer text-buffer))
  (with-output-to-string (out)
    (map nil (lambda (string)
               (write-string string out))
         (cluffer:items buffer))))
