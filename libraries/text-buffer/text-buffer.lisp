;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

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
    (cluffer:item-before-cursor cursor)))

(defmethod safe-backward ((cursor cursor))
  (unless (cluffer:beginning-of-line-p cursor)
    (cluffer:backward-item cursor)
    (cluffer:item-after-cursor cursor)))

(defmethod delete-item-forward ((cursor cursor))
  (unless (cluffer:end-of-line-p cursor)
    (cluffer:delete-item cursor)
    t))

(defmethod delete-item-backward ((cursor cursor))
  (unless (cluffer:beginning-of-line-p cursor)
    (cluffer:erase-item cursor)
    t))

(defmethod word-separation-chars-at-cursor-p ((cursor cursor) &key direction)
  "Return non-nil when `word-separation-characters' are found
before/after the cursor position. You can specify to look before or
after the cursor by supplying :backward or :forward for the direction
value."
  (find (cond ((and (not (cluffer:beginning-of-line-p cursor))
                    (eq direction :backward))
               (cluffer:item-before-cursor cursor))
              ((and (not (cluffer:end-of-line-p cursor))
                    (eq direction :forward))
               (cluffer:item-after-cursor cursor)))
        (word-separation-characters cursor)
        :test #'equal))

(defmethod move-to-word ((cursor cursor) &key direction conservative-word-move)
  "Move the cursor to the boundary of a word and return its
position. A word is a string bounded by
`word-separation-characters'. Specify a `direction' of :forward or
:backward to change the movement."
  (labels ((move-to-boundary (&key over-non-word-chars)
             "Move the cursor while it finds
              `word-separation-characters' adjacent to it. When
              `over-non-word-chars' is `t' move the cursor otherwise."
             (loop named movement
                   while (if over-non-word-chars
                             (word-separation-chars-at-cursor-p cursor :direction direction)
                             (not (word-separation-chars-at-cursor-p cursor :direction direction)))
                   unless (if (eq direction :backward)
                              (safe-backward cursor)
                              (safe-forward cursor))
                   do (return-from movement))))
    (if (word-separation-chars-at-cursor-p cursor :direction direction)
        (progn (move-to-boundary :over-non-word-chars t)
               (when conservative-word-move (move-to-boundary)))
        (move-to-boundary)))
  (cluffer:cursor-position cursor))

(defmethod move-forward-word ((cursor cursor) &key conservative-word-move)
  (move-to-word cursor :direction :forward
                       :conservative-word-move conservative-word-move))

(defmethod move-backward-word ((cursor cursor) &key conservative-word-move)
  (move-to-word cursor :direction :backward
                       :conservative-word-move conservative-word-move))

(defmethod delete-word ((cursor cursor) &key direction)
  "Delete characters until encountering the boundary of a
word. Specify a `direction' as :forward or :backward."
  (let ((start-cursor-position (cluffer:cursor-position cursor))
        (end-cursor-position
          (if (eq direction :backward)
              (move-backward-word cursor :conservative-word-move t)
              (move-forward-word cursor :conservative-word-move t))))
    (dotimes (i (abs (- start-cursor-position end-cursor-position)))
      (if (eq direction :backward)
          (cluffer:delete-item cursor)
          (cluffer:erase-item cursor)))))

(defmethod delete-forward-word ((cursor cursor))
  "Delete characters forward until encountering the end of a word."
  (delete-word cursor :direction :forward))

(defmethod delete-backward-word ((cursor cursor))
  "Delete characters backward until encountering the end of a word."
  (delete-word cursor :direction :backward))

(defmethod kill-forward-line ((cursor cursor))
  (loop while (delete-item-forward cursor)))

(defmethod insert-string ((cursor cursor) string)
  (loop for char across string do
        (cluffer:insert-item cursor (string char))))

(defmethod word-at-cursor ((cursor cursor))
    "Return word at cursor. If cursor is between two words, return the
first one."
  (let ((original-cursor-position (cluffer:cursor-position cursor)))
    (move-backward-word cursor)
    (let* ((delta (abs (- (cluffer:cursor-position cursor)
                          (move-forward-word cursor))))
           (word-at-cursor (reverse (apply #'concatenate 'string
                                           (loop repeat delta
                                                 collect (safe-backward cursor))))))
      (setf (cluffer:cursor-position cursor) original-cursor-position)
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
