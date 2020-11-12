;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :text-analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defclass document-vertex (document)
  ((edges :accessor edges :initform (make-hash-table)
          :documentation "The keys of the hash table represent the
          edges, the values of the hash table represent the edge
          weights.")))

(defmethod word-count-vectorize ((document document) dictionary)
  "Transform a document into a vector using word counts."
  (let ((vector-form (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-form)
          do (setf (aref vector-form index) (term-count document word)))
    (setf (vector-form document) vector-form)))

(defmethod tf-idf-vectorize ((document document) (collection document-collection) dictionary)
  "Transform a document into a vector using tf-idf."
  (let ((vector-form (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-form)
          do (setf (aref vector-form index)
                   (term-frequency-inverse-document-frequency document collection word)))
    (setf (vector-form document) vector-form)))

(defmethod word-count-vectorize-documents ((document-collection document-collection))
  "Set the vector-form for a collection of documents."
  (let ((dictionary (dictionary document-collection)))
    (loop for document in (documents document-collection)
          do (word-count-vectorize document dictionary))))

(defmethod tf-idf-vectorize-documents ((document-collection document-collection))
  "Set the vector-form for a collection of documents."
  (let ((dictionary (dictionary document-collection)))
    (loop for document in (documents document-collection)
          do (tf-idf-vectorize document document-collection dictionary))))

(defmethod cosine-similarity ((document-a document) (document-b document))
  "Calculate the cosine similarity between two vectors."
  (flet ((vector-product (document-a document-b)
           (loop for a across (vector-form document-a)
                 for b across (vector-form document-b)
                 sum (* a b)))
         (vector-sum-root (document)
           (sqrt (loop for i across (vector-form document)
                       sum (* i i))))
         (vector-zero-p (document)
           (every #'zerop (vector-form document))))
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
               (<=  (abs (- previous-score current-score)) epsilon)))
      (set-initial-rank)
      (let (converged)
        (loop for iteration from 0 to iteration-limit until converged
              do (setf converged t)
                 (loop for document in documents
                       do (unless (convergedp (rank document)
                                              (setf (rank document)
                                                    (loop for neighbor in (graph-neighbors document)
                                                          sum (/ (* damping (rank neighbor) (document-similarity document neighbor))
                                                                 (graph-neighbor-edge-sum neighbor)))))
                            (setf converged nil))))))))

(defun summarize-text (text &key (summary-length 3) (show-rank-p nil))
  (let ((collection (make-instance 'document-collection)))
    (loop for sentence in (sentence-tokenize text)
          do (add-document collection
                           (make-instance 'document-vertex
                                          :string-contents sentence)))
    (word-count-vectorize-documents collection)
    (generate-document-similarity-vectors collection)
    (text-rank collection :iteration-limit 100)
    (serapeum:take summary-length
                   (mapcar (if show-rank-p
                               (lambda (i) (cons (rank i) (string-contents i)))
                               #'string-contents)
                    (sort (documents collection) #'> :key #'rank)))))
