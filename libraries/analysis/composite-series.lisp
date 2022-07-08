;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

(defclass sequence-model (node)
  ()
  (:documentation "The sequence-model class represents the root of a directed
  graph. The edges represent possible sequences of events. It may help to
  envision the graph as a finite state machine."))

(defclass node ()
  ((edges :accessor edges :initform (make-hash-table :test #'equal))))

(defclass element-node (node)
  ((element :accessor element :initarg :element)
   (occurrences
    :accessor occurrences
    :initform 0
    :documentation "Number of times this element has appeared at the end of a
    sequence.")))

(defmethod add-edge ((from-node node) (to-node node))
  (alexandria:ensure-gethash (element to-node)
                             (edges from-node) to-node))

(defmethod list-edge-elements ((node node))
  (mapcar #'element (alexandria:hash-table-values (edges node))))

(defmethod increment ((node node))
  (incf (occurrences node)))

(defmethod add-record ((model sequence-model) sequence)
  (let* ((list-but-last-element (butlast sequence))
         (last-element (car (last sequence)))
         (leaf (alexandria:ensure-gethash list-but-last-element
                                          (edges model)
                                          (make-instance 'node))))
    (increment (add-edge leaf (make-instance 'element-node :element last-element)))))