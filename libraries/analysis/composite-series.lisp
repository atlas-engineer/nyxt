;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

(defclass sequence-model ()
  ((record :accessor record :initform (make-hash-table :test #'equal))))

(defclass element ()
  ((occurrence-count :accessor occurrence-count :initform 0)
   (element :accessor element :initarg :element)))

(defmethod increment ((element element))
  (setf (occurrence-count element) (incf (occurrence-count element))))

(defmethod add-record ((model sequence-model) sequence)
  (let* ((list-but-last-element (butlast sequence))
         (last-element (car (last sequence)))
         (record-result (setf (gethash list-but-last-element (record model))
                              (gethash list-but-last-element (record model)
                                       (make-instance 'element
                                                      :element last-element)))))
    (increment record-result)))
