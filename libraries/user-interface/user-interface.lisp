;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :user-interface)

(defvar *id* 0 "Counter used to generate a unique ID.")

(defun unique-id ()
  (format nil "ui-element-~d" (incf *id*)))

(defclass ui-element ()
  ((id :accessor id)
   (buffer :accessor buffer :initarg :buffer
           :documentation "Buffer where element is drawn.")))

(defmethod initialize-instance :after ((element ui-element) &key)
  (setf (id element) (unique-id)))

(defmethod object-string ((element ui-element))
  ;; TODO: figure out how to remove `eval'
  (eval `(spinneret:with-html-string
           ,(object-expression element))))

(defmethod connect ((element ui-element) buffer)
  (setf (buffer element) buffer))

(defgeneric update (ui-element)
  (:documentation "Propagate changes to the buffer."))

(defclass button (ui-element)
  ((text :initform "" :initarg :text :accessor text)
   (alt-text :initform "" :initarg :alt-text :accessor alt-text)
   (action :initform ""  :initarg :action :accessor action)))

(defmethod (setf text) :after (text (button button))
  (declare (ignorable text))
  (when (slot-boundp button 'buffer)
    (update button)))

(defmethod (setf action) :after (action (button button))
  (declare (ignorable action))
  (when (slot-boundp button 'buffer)
    (update button)))

(defmethod (setf alt-text) :after (text (button button))
  (declare (ignorable text))
  (when (slot-boundp button 'buffer)
    (update button)))

(defmethod object-expression ((button button))
  `(:button :id ,(id button)
            :class "button"
            :title ,(alt-text button)
            :onclick ,(action button)
            ,(text button)))

(defclass paragraph (ui-element)
  ((text :initform "" :initarg :text :accessor text)))

(defmethod (setf text) :after (text (paragraph paragraph))
  (declare (ignorable text))
  (when (slot-boundp paragraph 'buffer)
    (update paragraph)))

(defmethod object-expression ((paragraph paragraph))
  `(:p :id ,(id paragraph) ,(text paragraph)))

(defclass progress-bar (ui-element)
  ((percentage :initform 0
               :initarg :percentage
               :accessor percentage
               :documentation "The percentage the progress bar is
filled up, use a number between 0 and 100.")))

(defmethod object-expression ((progress-bar progress-bar))
  `(:div :class "progress-bar-base"
         (:div :class "progress-bar-fill"
               :id ,(id progress-bar)
               ;; empty string to force markup to make closing :div tag
               "")))

(defmethod (setf percentage) :after (percentage (progress-bar progress-bar))
  (declare (ignorable percentage))
  (when (slot-boundp progress-bar 'buffer)
    (update progress-bar)))
