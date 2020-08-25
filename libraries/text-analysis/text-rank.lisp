(in-package :text-analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  (mapcar #'str:trim (cl-ppcre:split "\\.|\\?|\\!" string)))

(defclass document-matrix (document-collection)
  ((document-matrix :accessor matrix
                    :documentation "Matrix showing the vector
    relationships between each document."))
  (:documentation "A graph with vector and matrix representation used
  to rank the most important documents in a collection."))

(defmethod word-count-vectorize ((document document) dictionary)
  "Transform a document into a vector using word counts."
  (let ((vector-form (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-form)
          do (setf (aref vector-form index) (term-count document word)))
    (setf (vector-form document) vector-form)))

(defmethod word-count-vectorize-documents ((document-collection document-collection))
  (let ((dictionary (dictionary document-collection)))
    (loop for document in (documents document-collection)
          do (word-count-vectorize document dictionary))))

(defmethod cosine-similarity ((document-a document) (document-b document))
  (flet ((vector-product (document-a document-b)
           (loop for a across (vector-form document-a)
                 for b across (vector-form document-b)
                 sum (* a b)))
         (vector-sum-root (document)
           (sqrt (loop for i across (vector-form document)
                       sum (* i i)))))
    (/ (vector-product document-a document-b)
       (* (vector-sum-root document-a) (vector-sum-root document-b)))))

(defmethod generate-document-similarity-matrix ((collection document-matrix))
  (setf (matrix collection)
        (make-array (list (length (documents collection))
                          (length (documents collection)))
                    :initial-element 0))
  (loop for x in (documents collection)
        for ix from 0 below (length (documents collection)) do
           (loop for y in (documents collection)
                 for iy from 0 below (length (documents collection)) do
                    (setf (aref (matrix collection) ix iy)
                          (cosine-similarity x y)))))

(defmethod text-rank ((collection document-matrix) &key (epsilon 0.001)
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
  (loop for document in (documents collection) do
           (setf (rank document)
                 (or initial-rank
                     (/ 1 (length (documents collection))))))
  (with-accessors ((documents documents)) collection
    (labels ((graph-neighbor-ids (node-id)
               "Return a list of neighbors. In a fully connected graph,
              all nodes are a neighbor except for the node itself."
               (loop for i from 0 below (length (documents collection))
                     when (/= i node-id)
                     collect i))
             (graph-neighbor-edge-sum (node-id)
               "Add up the edges of all neighbors of a given node."
               (loop for neighbor in (graph-neighbor-ids node-id)
                     sum (aref (matrix collection) node-id neighbor)))
             (converged-p (previous-score current-score)
               "Check if a delta qualifies for convergence."
               (if (<=  (abs (- previous-score current-score)) epsilon)
                   t
                   nil)))
      (let (converged)
        (loop for iterations from 0 until (or converged (= iterations iteration-limit)) do
                 (setf converged t)
                 (loop for x from 0 below (length (documents collection))
                       do (let ((previous-rank (rank (nth x (documents collection)))))
                            (setf (rank (nth x documents)) (- 1 damping))
                            (loop for y in (graph-neighbor-ids x)
                                  do (setf (rank (nth x documents))
                                           (/ (* damping
                                                 (rank (nth y documents))
                                                 (aref (matrix collection) x y))
                                              (graph-neighbor-edge-sum y)))
                                     (unless (converged-p previous-rank
                                                          (rank (nth x (documents collection))))
                                       (setf converged nil))))))))))
