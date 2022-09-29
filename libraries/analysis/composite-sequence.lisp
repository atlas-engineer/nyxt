;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;; Given the following sequence:
;; 1 2 3 4 5
;;
;; We would record the following sequences/outcomes.
;;
;; Chain length of 1:
;; 1 -> 2
;; 2 -> 3
;; 3 -> 4
;; 4 -> 5
;;
;; Chain length of 2:
;; 1 2 -> 3
;; 2 3 -> 4
;; 3 4 -> 5
;;
;; Chain length of 3:
;; 1 2 3 -> 4
;; 2 3 4 -> 5
;;
;; As can be seen above, the amount of subsequences within a given sequence is
;; equal to (- (length sequence) chain length).

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
  (multiple-value-bind (list-but-last-element last-element) (serapeum:halves sequence)
    (let ((leaf (alexandria:ensure-gethash list-but-last-element
                                           (edges model)
                                           (make-instance 'node))))
      (increment (add-edge leaf (make-instance 'element-node :element (first last-element)))))))

(defmethod add-record-subsequence ((model sequence-model) sequence)
  "Add a record for all subsequences. E.g. transform '(3 2 1)' into:
'(3 2 1), '(2 1), '(1)"
  (loop while (> (length sequence) 1)
        collect (add-record model sequence)
        do (setf sequence (rest sequence))))

(defmethod predict ((model sequence-model) sequence)
  (let* ((leaf (gethash sequence (edges model)))
         (edges (alexandria:hash-table-values (edges leaf))))
    (first (sort edges #'> :key #'occurrences))))

(defmethod predict-subsequence-simple ((model sequence-model) sequence)
  "Predict a sequence's next value based on all subsequence predictions. This is
a naive implementation which simply considers the amount of occurences without
regard to the weight of different chain lengths."
  (let* ((subsequence-results
           (loop while (> (length sequence) 1)
                 collect (let* ((leaf (gethash sequence (edges model)))
                                (edges (alexandria:hash-table-values (edges leaf))))
                           (first (sort edges #'> :key #'occurrences)))
                 do (setf sequence (rest sequence)))))
    (first (sort subsequence-results #'> :key #'occurrences))))
