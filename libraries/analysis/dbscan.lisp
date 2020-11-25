;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

;;; dbscan.lisp -- implementation of Density-based spatial clustering
;;; of applications with noise (DBSCAN) algorithm

(defclass document-cluster (document-vertex)
  ((cluster :accessor cluster)))

(defmethod distance ((vector-1 t) (vector-2 t))
  "Calculate the euclidean distance between two vectors."
  (sqrt (loop for i across vector-1
              for j across vector-2
              sum (expt (- i j) 2))))

(defmethod generate-document-distance-vectors ((collection document-collection))
  "Set the edge weights for all document neighbors (graph is fully connected)."
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (loop for document-b in documents
                   do (setf (gethash document-b (edges document-a))
                            (distance document-a document-b))))))

(defmethod dbscan ((collection document-collection) &key (minimum-points 3)
                                                         (epsilon 0.5))
  (labels ((range-query (document)
             "Return all points that have a distance less than epsilon."
             (loop for vertex being the hash-keys of (edges document)
                   when (and (<= (gethash vertex (edges document)) epsilon)
                             (not (eq vertex document)))
                   collect vertex)))
    ))
