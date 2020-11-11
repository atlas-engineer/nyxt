;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :text-analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defclass document-similarity-vector (document)
  ((similarity-vector :accessor similarity-vector)))

(defmethod word-count-vectorize ((document document) dictionary)
  "Transform a document into a vector using word counts."
  (let ((vector-form (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-form)
          do (setf (aref vector-form index) (term-count document word)))
    (setf (vector-form document) vector-form)))

(defmethod tf-idf-vectorize ((document document) (collection document-collection) dictionary)
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
  (with-accessors ((documents documents)) collection
    (loop for document-a in documents
          do (setf (similarity-vector document-a)
                   (loop for document-b in documents
                         collect (cons (cosine-similarity document-a document-b) document-b))))))

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
  (declare (ignore epsilon))
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
               (let ((sum (- (reduce #'+ (mapcar #'car (similarity-vector document))) 1)))
                 (if (> sum 0) sum 1)))
             (document-similarity (document-a document-b)
               (car (find document-b (similarity-vector document-a) :key #'cdr))))
      (set-initial-rank)
      (loop for iteration from 0 to iteration-limit
            do (loop for document in documents
                     do (setf (rank document)
                              (loop for neighbor in (graph-neighbors document)
                                    sum (/ (* damping (rank neighbor) (document-similarity document neighbor))
                                           (graph-neighbor-edge-sum neighbor)))))))))
