;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

;;; dbscan.lisp -- implementation of Density-based spatial clustering
;;; of applications with noise (DBSCAN) algorithm

(defclass document-cluster (document-vertex)
  ((cluster :accessor cluster)
   (neighbors :accessor neighbors)))

(defmethod distance ((vector-1 t) (vector-2 t))
  "Calculate the euclidean distance between two vectors."
  (sqrt (loop for i across vector-1
              for j across vector-2
              sum (expt (- i j) 2))))

(defmethod distance ((document-a document-cluster) (document-b document-cluster))
  (distance (vector-form document-a) (vector-form document-b)))

(defmethod generate-document-distance-vectors ((collection document-collection))
  "Set the edge weights for all document neighbors (graph is fully connected)."
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (loop for document-b in documents
                   do (setf (gethash document-b (edges document-a))
                            (distance document-a document-b))))))

(defmethod dbscan ((collection document-collection) &key (minimum-points 3)
                                                         (epsilon 0.5))
  "DBSCAN(DB, distFunc, eps, minPts) {
       C := 0                                                  /* Cluster counter */
       for each point P in database DB {
           if label(P) ≠ undefined then continue               /* Previously processed in inner loop */
           Neighbors N := RangeQuery(DB, distFunc, P, eps)     /* Find neighbors */
           if |N| < minPts then {                              /* Density check */
               label(P) := Noise                               /* Label as Noise */
               continue
           }
           C := C + 1                                          /* next cluster label */
           label(P) := C                                       /* Label initial point */
           SeedSet S := N \ {P}                                /* Neighbors to expand */
           for each point Q in S {                             /* Process every seed point Q */
               if label(Q) = Noise then label(Q) := C          /* Change Noise to border point */
               if label(Q) ≠ undefined then continue           /* Previously processed (e.g., border point) */
               label(Q) := C                                   /* Label neighbor */
               Neighbors N := RangeQuery(DB, distFunc, Q, eps) /* Find neighbors */
               if |N| ≥ minPts then {                          /* Density check (if Q is a core point) */
                   S := S ∪ N                                  /* Add new neighbors to seed set */
               }
           }
       }
   }
"
  (labels ((range-query (document)
             "Return all points that have a distance less than epsilon."
             (loop for vertex being the hash-keys of (edges document)
                   when (and (<= (gethash vertex (edges document)) epsilon)
                             (not (eq vertex document)))
                   collect vertex))
           (noisep (document)
             "A document must have a minimum number of neighbors to
              not qualify as noise."
             (setf (neighbors document) (range-query document))
             (if (< (+ 1 (length (neighbors document))) minimum-points)
                 (progn (setf (cluster document) :noise) t)
                 nil)))
    (loop for document in (documents collection)
          with cluster = 0
          with stack = (list)
          unless (or (slot-boundp document 'cluster)
                     (noisep document))
          do (incf cluster)
             (setf (cluster document) cluster)
             (setf stack (neighbors document))
             (loop while stack for item = (pop stack)
                   when (and (slot-boundp item 'cluster)
                             (eq (cluster item) :noise))
                   do (setf (cluster item) cluster)
                   unless (slot-boundp item 'cluster)
                   do (setf (cluster item) cluster)
                      (unless (noisep item)
                        (append (neighbors item) stack))))))
