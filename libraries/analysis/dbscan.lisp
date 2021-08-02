;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

;;; dbscan.lisp -- implementation of Density-based spatial clustering
;;; of applications with noise (DBSCAN) algorithm

(defclass document-cluster (document-vertex)
  ((cluster :accessor cluster :initform :noise)
   (neighbors :accessor neighbors))
  (:documentation "The document cluster class represents a document
that is part of a graph which will be clustered. It extends the
documenet-vertex class and adds support for a cluster tag and a list
of neighbors. These slots are useful for clustering algorithms."))

(defmethod clusters ((collection document-collection))
  "Return a list of clusters. Each hash key represents a cluster, and
   the hash value is the list of elements in that cluster.

   Please note: this function is not responsible for computing the
   clusters, only for returning the list of pre-tagged documents in
   cluster lists."
  (let ((result (make-hash-table)))
    (loop for document in (documents collection)
          do (push document (gethash (cluster document) result (list))))
    result))

(defun get-cluster (cluster-label points)
  "Return all matching points for a given cluster label."
  (remove-if-not (lambda (i) (eq (cluster i) cluster-label)) points))

(defmethod distance ((vector-1 t) (vector-2 t))
  "Return the Euclidean distance between two vectors."
  (sqrt (loop for i across vector-1
              for j across vector-2
              sum (expt (- i j) 2))))

(defmethod distance ((document-a document-cluster) (document-b document-cluster))
  (distance (vector-data document-a) (vector-data document-b)))

(defmethod generate-document-distance-vectors ((collection document-collection))
  "Set the edge weights for all document neighbors (graph is fully connected)."
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (loop for document-b in documents
                   do (setf (gethash document-b (edges document-a))
                            (distance document-a document-b))))))

(defmethod dbscan ((collection document-collection) &key (minimum-points 3)
                                                         (epsilon 0.5))
  "Minimum points refers to the minimum amount of points that must
   exist in the neighborhood of a point for it to be considered a
   core-point in a cluster. Epsilon refers to the distance between
   two points for them to be considered neighbors."
  (labels ((range-query (document)
             "Return all points that have a distance less than epsilon."
             (loop for vertex being the hash-keys of (edges document)
                   when (and (<= (gethash vertex (edges document)) epsilon)
                             (not (eq vertex document)))
                   collect vertex))
           (core-point-p (point)
             "Is a point a core-point?"
             (<= minimum-points (length (range-query point))))
           (cluster-match-p (point cluster)
             "Check if a core point belongs to a cluster."
             (intersection cluster (range-query point))))
    ;;; identify core points
    (let* ((core-points (remove-if-not #'core-point-p (documents collection)))
           (non-core-points (set-difference (documents collection) core-points)))
      ;;; assign labels to core points
      (loop for point in core-points
            with cluster-count = 0
            do (loop named cluster-set
                     for i from 0 to cluster-count
                     ;; point found cluster match, setf and break
                     when (cluster-match-p point (get-cluster i core-points))
                     do (setf (cluster point) i)
                        (return-from cluster-set)
                     ;; point found no cluster-match, create new cluster
                     finally (setf (cluster point) (incf cluster-count))))
      ;;; assign labels to non-core points
      (loop for point in non-core-points
            for intersection = (intersection core-points (range-query point))
            when intersection
            do (setf (cluster point) (cluster (first intersection)))))))
