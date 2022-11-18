;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defclass document-vertex (document)
  ((edges :accessor edges :initform (make-hash-table)
          :documentation "The keys of the hash table represent the
          edges, the values of the hash table represent the edge
          weights."))
  (:documentation "The document vertex class represents a document
that is part of a graph. The edges slot of the document vertex class
is used to store edges of that particular vertex. The keys in the
edges slot hash table are the actual vertexes, and the values are the
edge weights."))

(defmethod cosine-similarity ((document-a document) (document-b document))
  "Calculate the cosine similarity between two vectors."
  (flet ((vector-product (document-a document-b)
           (loop for a across (vector-data document-a)
                 for b across (vector-data document-b)
                 sum (* a b)))
         (vector-sum-root (document)
           (sqrt (loop for i across (vector-data document)
                       sum (* i i))))
         (vector-zero-p (document)
           (every #'zerop (vector-data document))))
    (if (or (vector-zero-p document-a) (vector-zero-p document-b))
        0 ; if either vector is completely zero, they are dissimilar
        (/ (vector-product document-a document-b)
           (* (vector-sum-root document-a) (vector-sum-root document-b))))))

(defmethod generate-document-similarity-vectors ((collection document-collection))
  "Set the edge weights for all document neighbors (graph is fully connected)."
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (loop for document-b in documents
                   do (setf (gethash document-b (edges document-a))
                            (cosine-similarity document-a document-b))))))

(defmethod text-rank ((collection document-collection) &key (epsilon 0.001)
                                                            (damping 0.85)
                                                            (initial-rank)
                                                            (iteration-limit 100))
  "This method is used to calculate the text rankings for a document
   collection. The `epsilon' is the maximum delta for a given node
   rank change during an iteration to be considered convergent. The
   `damping' is a factor utilized to normalize the data. The
   `initial-rank' is the rank given to nodes before any
   iterations. The `iteration-limit' is the amount of times the
   algorithm may traverse the graph before giving up (if the algorithm
   does not converge)."
  (with-accessors ((documents documents)) collection
    (unless (zerop (length documents))
      (labels ((set-initial-rank ()
                 "Set the initial rank of all documents to a supplied
                value OR 1/length of the documents."
                 (let ((initial-rank (or initial-rank (/ 1 (length documents)))))
                   (mapcar (lambda (document) (setf (rank document) initial-rank)) documents)))
               (graph-neighbors (document)
                 "Return a list of neighbors. In a fully connected graph,
                all nodes are a neighbor except for the node itself."
                 (remove document documents))
               (graph-neighbor-edge-sum (document)
                 "Add up the edges of all neighbors of a given node."
                 (let ((sum (- (reduce #'+ (alexandria:hash-table-values (edges document))) 1)))
                   (if (> sum 0) sum 1)))
               (document-similarity (document-a document-b)
                 (gethash document-b (edges document-a) 0))
               (convergedp (previous-score current-score)
                 "Check if a delta qualifies for convergence."
                 (<=  (abs (- previous-score current-score)) epsilon))
               (calculate-rank (document)
                 "Calculate the rank of a document."
                 (loop for neighbor in (graph-neighbors document)
                       sum (/ (* damping (rank neighbor) (document-similarity document neighbor))
                              (graph-neighbor-edge-sum neighbor)))))
        (set-initial-rank)
        (loop with converged = nil
              for iteration from 0 to iteration-limit until converged
              do (setf converged t)
                 (loop for document in documents
                       for old-rank = (rank document)
                       for new-rank = (calculate-rank document)
                       do (setf (rank document) new-rank)
                       unless (convergedp old-rank new-rank)
                       do (setf converged nil)))))))

(export-always 'summarize-text)
(defun summarize-text (text &key (summary-length 3) (show-rank-p nil))
  (let ((collection (make-instance 'document-collection)))
    (loop for sentence in (sentence-tokenize text)
          do (add-document collection
                           (make-instance 'document-vertex
                                          :string-contents sentence)))
    (tf-idf-vectorize-documents collection)
    (generate-document-similarity-vectors collection)
    (text-rank collection :iteration-limit 100)
    (serapeum:take summary-length
                   (mapcar (if show-rank-p
                               (lambda (i) (cons (rank i) (string-contents i)))
                               #'string-contents)
                           (sort (documents collection) #'> :key #'rank)))))
