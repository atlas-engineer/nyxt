;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :user-interface)

(defclass button ()
  ((text :initform "" :accessor text :initarg :text)
   (url :initform "" :accessor url :initarg :url)))

(defmethod object-string ((button button))
  (cl-markup:markup
   (:a :class "button" :href (url button) (text button))))

