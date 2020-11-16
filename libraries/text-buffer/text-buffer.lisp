;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause



(in-package :text-buffer)

(defclass text-buffer (cluffer-simple-line:line) ())

(defclass cursor (cluffer-simple-line::right-sticky-cursor)
  ((word-separation-characters
    :accessor word-separation-characters
    :initform '(":" "/" "." " " " "))))

(defvar conservative-word-move nil
  "If non-nil, the cursor moves to the end (resp. beginning) of the word
  when `move-forward-word' (resp. `move-backward-word') is called.")

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

(defmethod delete-item-forward ((cursor cursor))
  (unless (cluffer:end-of-line-p cursor)
    (cluffer:delete-item cursor)
    t))

(defmethod delete-item-backward ((cursor cursor))
  (unless (cluffer:beginning-of-line-p cursor)
    (cluffer:erase-item cursor)
    t))

(defmethod word-separation-chars-at-cursor-p ((cursor cursor) &key before)
  "Return non-nil when `word-separation-characters' are found after the
cursor position.

When `before' is `t', look before the cursor."
  (find (if before
            (cluffer:item-before-cursor cursor)
            (cluffer:item-after-cursor cursor))
        (word-separation-characters cursor)
        :test #'equal))

(defmethod move-to-word ((cursor cursor) &key backward
                                           (conservative-word-move
                                           conservative-word-move))
  "Move the cursor to the boundary of a word and return its position.

A word is a string bounded by `word-separation-characters'."
  (labels
      ((line-limits-p ()
         (if backward
             (cluffer:beginning-of-line-p cursor)
             (cluffer:end-of-line-p cursor)))
       (word-separation-chars-p ()
         (apply #'word-separation-chars-at-cursor-p
                cursor (when backward '(:before t))))
       (move-to-boundary (&key over-non-word-chars)
         "Move the cursor while it finds `word-separation-characters'
adjacent to it.

When `over-non-word-chars' is `t' move the cursor otherwise."
         (unless (line-limits-p)
           (loop while (if over-non-word-chars
                           (word-separation-chars-p)
                           (not (word-separation-chars-p)))
                 do (if backward
                        (cluffer:backward-item cursor)
                        (cluffer:forward-item cursor))
                 until (line-limits-p)))))
    (unless (line-limits-p)
      (if (word-separation-chars-p)
          (progn (move-to-boundary :over-non-word-chars t)
                 (when conservative-word-move (move-to-boundary)))
          (move-to-boundary))))
  (cluffer:cursor-position cursor))

(defmethod move-forward-word ((cursor cursor)
                              &key (conservative-word-move
                                    conservative-word-move))
  (move-to-word cursor))

(defmethod move-backward-word ((cursor cursor)
                               &key (conservative-word-move
                                     conservative-word-move))
  (move-to-word cursor :backward t))

(defmethod delete-word ((cursor cursor) &key backward)
  "Delete characters until encountering the boundary of a word."
  (let ((beg (cluffer:cursor-position cursor))
        (end (if backward
                 (move-backward-word cursor :conservative-word-move t)
                 (move-forward-word cursor :conservative-word-move t))))
    (when (numberp end)
      (loop repeat (abs (- beg end))
            do (if backward
                   (cluffer:delete-item cursor)
                   (cluffer:erase-item cursor))))))

(defmethod delete-forward-word ((cursor cursor))
  "Delete characters forward until encountering the end of a word."
  (delete-word cursor))

(defmethod delete-backward-word ((cursor cursor))
  "Delete characters backward until encountering the end of a word."
  (delete-word cursor :backward t))

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
