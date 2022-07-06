;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

(defclass sequence-model ()
  ((sequence-look-up :accessor sequence-look-up :initform (make-hash-table :test #'equal))))

(defclass node ()
  ((edges :accessor edges :initform (make-hash-table :test #'equal))
   (occurrence-count :accessor occurrence-count :initform 0)
   (element :accessor element :initarg :element)))

(defmethod add-edge ((from-node node) (to-node node))
  (alexandria:ensure-gethash (element to-node)
                             (edges from-node) to-node))

(defmethod list-edge-elements ((node node))
  (mapcar #'element (alexandria:hash-table-values (edges node))))

(defmethod increment ((node node))
  (setf (occurrence-count node) (incf (occurrence-count node))))

(defmethod add-record ((model sequence-model) sequence)
  (let* ((list-but-last-element (butlast sequence))
         (last-element (car (last sequence)))
         (leaf (alexandria:ensure-gethash list-but-last-element
                                          (sequence-look-up model)
                                          (make-instance 'node))))
    (increment (add-edge leaf (make-instance 'node :element last-element)))))
