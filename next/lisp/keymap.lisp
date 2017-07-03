(in-package :next)

(qadd-event-filter nil |QEvent.KeyPress| 'key-press)
(qadd-event-filter nil |QEvent.KeyRelease| 'key-release)

(defvar *control-modifier* nil
  "Is the control modifier active")

(defun kbd (keys)
  "Convert KEYS to the internal next representation")

(defun key-press (obj event)
  (case (|key| event)
    (#.|Qt.Key_Control|
       (setf *control-modifier* t))
    (t ; all other keys
     (print (|text| event)))))

(defun key-release (obj event)
  (case (|key| event)
    (#.|Qt.Key_Control|
       (setf *control-modifier* nil))
    (t (return-from key-release))))

