;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :user-interface)

(defvar *id* 0 "Counter used to generate a unique ID.")

(defun unique-id ()
  (incf *id*))

(defclass ui-element ()
  ((id :accessor id)
   (buffer :accessor buffer :initarg :buffer
           :documentation "Buffer where element is drawn.")))

(defmethod initialize-instance :after ((element ui-element) &key)
  (setf (id element) (unique-id)))

(defmethod object-string ((element ui-element))
  (cl-markup:markup*
   (object-expression element)))

(defclass button (ui-element)
  ((text :initform "" :accessor text :initarg :text)
   (url :initform "" :accessor url :initarg :url)))

(defmethod object-expression ((button button))
  `(:a :class "button" :href ,(url button) ,(text button)))

(defclass paragraph (ui-element)
  ((text :initform "" :initarg :text)))

(defmethod (setf text) (text (paragraph paragraph))
  (setf (slot-value paragraph 'text) text))

(defmethod object-expression ((paragraph paragraph))
  `(:p :id ,(id paragraph) ,(text paragraph)))

(defmethod text ((paragraph paragraph))
  (slot-value paragraph 'text))
