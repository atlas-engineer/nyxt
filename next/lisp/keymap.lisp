(in-package :next)
(ql:quickload :cl-strings)
(use-package :cl-strings)

(defvar *control-modifier* nil
  "A variable to store the status of the control key")
(defvar *meta-modifier* nil
  "A variable to store the status of the alt/meta key")
(defvar *super-modifier* nil
  "A variable to store the status of the super/cmd key")

(defparameter global-map (make-hash-table :test 'equalp)
  "A global key map")
(defparameter *key-sequence-stack* ()
  "A buffer that keeps track of the keys a user has inputted")

(defstruct key
  character
  control-modifier)

(qadd-event-filter nil |QEvent.KeyPress| 'key-press)
(qadd-event-filter nil |QEvent.KeyRelease| 'key-release)

(defun key-press (obj event)
  (case (|key| event)
    (#.|Qt.Key_Control|
       (setf *control-modifier* t))
    (t ; all other keys
     (progn
       (push-key-chord (|text| event))
       (consume-key-sequence))))
  t ; return true to avoid propagation
  )

(defun key-release (obj event)
  (case (|key| event)
    (#.|Qt.Key_Control|
       (setf *control-modifier* nil))
    (t (return-from key-release))))

(defun push-key-chord (key-character-string)
  (let ((key-chord (make-key)))
    (if *control-modifier*
	(setf (key-control-modifier key-chord) t))
    
    (setf (key-character key-chord) key-character-string)
    (push key-chord *key-sequence-stack*)))

(defun consume-key-sequence ()
  ;; If key recognized, execute function
  (if (gethash *key-sequence-stack* global-map)
      (progn
	(funcall (gethash *key-sequence-stack* global-map))
	(setf *key-sequence-stack* ()))
      ;; If key not recognized, print message
      (progn
	(print "Key Undefined")
	(setf *key-sequence-stack* ()))))

(defun define-key (mode-map key-sequence function)
  (setf (gethash key-sequence mode-map) function))

(defun kbd (key-sequence-string)
  (let ((key-sequence ()))
    ;; Iterate through all key chords (space delimited)
    (loop for key-chord-string in (split key-sequence-string " ")
       ;; Iterate through all keys in chord (hyphen delimited)
       do (let ((key-chord (make-key)))
	    (loop for key-character-string in (split key-chord-string "-")
	       do (cond
		    ((equal "C" key-character-string) (setf (key-control-modifier key-chord) t))
		    (t (setf (key-character key-chord) key-character-string))))
	    (push key-chord key-sequence)))
    key-sequence))

;; create hash map of key to actual keys
;; http://doc.qt.io/qt-4.8/qt.html#Key-enum
;; use form #.|Qt.Key_Control|

(defun print-hey ()
  (print "hey"))

(define-key global-map (kbd "a") #'print-hey)
