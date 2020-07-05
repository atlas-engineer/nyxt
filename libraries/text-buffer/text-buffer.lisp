(in-package :text-buffer)

(defclass text-buffer (cluffer-simple-line:line) ())

(defclass cursor (cluffer-simple-line::right-sticky-cursor)
  ((word-separation-characters
    :accessor word-separation-characters
    :initform '(":" "/" "." " " " "))))

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
    (values
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
               collect (cluffer:item-before-cursor cursor)
               until (cluffer:end-of-line-p cursor)))
     (cluffer:cursor-position cursor))))

(defmethod move-backward-word ((cursor cursor))
  (unless (cluffer:beginning-of-line-p cursor)
    (values
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
               until (cluffer:beginning-of-line-p cursor)
               collect (cluffer:item-before-cursor cursor)))
     (cluffer:cursor-position cursor))))

(defmethod delete-backward-word ((cursor cursor))
  (dotimes (i (- (cluffer:cursor-position cursor) (or (nth-value 1 (move-backward-word cursor)) 0)))
    (cluffer:delete-item cursor)))

(defmethod delete-forward-word ((cursor cursor))
  (dotimes (i (abs (- (cluffer:cursor-position cursor) (or (nth-value 1 (move-forward-word cursor)) 0))))
    (delete-item-backward cursor)))

(defmethod kill-forward-line ((cursor cursor))
  (loop while (delete-item-forward cursor)))

(defmethod insert-string ((cursor cursor) string)
  (loop for char across string do
        (cluffer:insert-item cursor (string char))))

(defmethod word-at-cursor ((cursor cursor))
    "Return word at cursor.
If cursor is between two words, return the first one."
  (let ((cursor-position (cluffer:cursor-position cursor)))
    (move-backward-word cursor)
    (let ((word-at-cursor (apply #'concatenate 'string (move-forward-word cursor))))
      (setf (cluffer:cursor-position cursor) cursor-position)
      word-at-cursor)))

(defmethod replace-word-at-cursor ((cursor cursor) string)
  (unless (uiop:emptyp (word-at-cursor cursor))
    (move-backward-word cursor)
    (delete-forward-word cursor))
  (insert-string cursor string))

(defmethod kill-line ((cursor cursor))
  "Kill the complete line."
  (cluffer:beginning-of-line cursor)
  (kill-forward-line cursor))

(defun word-start (s position &optional (white-spaces '(#\space #\no-break_space)))
  "Return the index of the beginning word at POSITION in string S."
  (apply #'max
         (mapcar (lambda (char)
                   (let ((pos (position char s
                                        :end position
                                        :from-end t)))
                     (if pos
                         (1+ pos)
                         0)))
                 white-spaces)))

(defun word-end (s position &optional (white-spaces '(#\space #\no-break_space)))
  "Return the index of the end of the word at POSITION in string S."
  (apply #'min
         (mapcar (lambda (char)
                   (or (position char s :start position)
                       (length s)))
                 white-spaces)))

