(in-package :text-buffer)

(defclass text-buffer (cluffer-simple-line:line) ())

(defclass cursor (cluffer-simple-line::right-sticky-cursor)
  ((word-separation-characters
    :accessor word-separation-characters
    :initform '(":" "/" "-" "." " " " "))))

(defmethod string-representation ((buffer text-buffer))
  (with-output-to-string (out)
    (map nil (lambda (string)
               (write-string string out))
         (cluffer:items buffer))))

(defmethod invisible-string-representation ((buffer text-buffer))
  (make-string (cluffer:item-count buffer) :initial-element #\*))

(defmethod safe-forward ((cursor cursor))
  (unless (cluffer:end-of-line-p cursor)
    (cluffer:forward-item cursor)
    t))

(defmethod safe-backward ((cursor cursor))
  (unless (cluffer:beginning-of-line-p cursor)
    (cluffer:backward-item cursor)
    t))

(defmethod delete-item-backward ((cursor cursor))
  (when (safe-backward cursor)
    (cluffer:delete-item cursor)
    t))

(defmethod delete-item-forward ((cursor cursor))
  (unless (cluffer:end-of-line-p cursor)
    (cluffer:delete-item cursor)
    t))

(defmethod move-forward-word ((cursor cursor))
  (unless (cluffer:end-of-line-p cursor)
    (if (intersection (word-separation-characters cursor)
                      (list (cluffer:item-after-cursor cursor))
                      :test #'equal)
        (loop while (intersection (word-separation-characters cursor)
                                  (list (cluffer:item-after-cursor cursor))
                                  :test #'equal)
              do (cluffer:forward-item cursor)
              until (cluffer:end-of-line-p cursor))
        (loop while (not (intersection (word-separation-characters cursor)
                                       (list (cluffer:item-after-cursor cursor))
                                       :test #'equal))
              do (cluffer:forward-item cursor)
              until (cluffer:end-of-line-p cursor))))
  (cluffer:cursor-position cursor))

(defmethod move-backward-word ((cursor cursor))
  (unless (cluffer:beginning-of-line-p cursor)
    (if (intersection (word-separation-characters cursor)
                      (list (cluffer:item-before-cursor cursor))
                      :test #'equal)
        (loop while (intersection (word-separation-characters cursor)
                                  (list (cluffer:item-before-cursor cursor))
                                  :test #'equal)
              do (cluffer:backward-item cursor)
              until (cluffer:beginning-of-line-p cursor))
        (loop while (not (intersection (word-separation-characters cursor)
                                       (list (cluffer:item-before-cursor cursor))
                                       :test #'equal))
              do (cluffer:backward-item cursor)
              until (cluffer:beginning-of-line-p cursor))))
  (cluffer:cursor-position cursor))

(defmethod delete-backward-word ((cursor cursor))
  (dotimes (i (- (cluffer:cursor-position cursor) (move-backward-word cursor)))
    (cluffer:delete-item cursor)))

(defmethod delete-forward-word ((cursor cursor))
  (dotimes (i (abs (- (cluffer:cursor-position cursor) (move-forward-word cursor))))
    (delete-item-backward cursor)))

(defmethod kill-forward-line ((cursor cursor))
  (loop while (delete-item-forward cursor)))

(defmethod insert-string ((cursor cursor) string)
  (loop for char across string do
        (cluffer:insert-item cursor (string char))))

(defmethod word-start ((cursor cursor) position)
  "Return the index of the beginning word at POSITION."
  (let* ((cursor-position (cluffer:cursor-position cursor))
         (word-beginning (progn (setf (cluffer:cursor-position cursor) position)
                                (move-backward-word cursor))))
    (setf (cluffer:cursor-position cursor) cursor-position)
    word-beginning))

(defmethod word-end ((cursor cursor) position)
  "Return the index of the end word at POSITION."
  (let* ((cursor-position (cluffer:cursor-position cursor))
         (word-end (progn (setf (cluffer:cursor-position cursor) position)
                          (move-forward-word cursor))))
    (setf (cluffer:cursor-position cursor) cursor-position)
    word-end))

(defmethod word-at-cursor ((cursor cursor))
    "Return word at cursor.
If cursor is between two words, return the first one.")

(defmethod kill-line ((cursor cursor))
  "Kill the complete line."
  (cluffer:beginning-of-line cursor)
  (kill-forward-line cursor))
