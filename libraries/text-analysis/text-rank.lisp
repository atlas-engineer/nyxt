(in-package :text-analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  (mapcar #'str:trim (cl-ppcre:split "\\.|\\?|\\!" string)))

(defclass document-graph (document-collection)
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

(defmethod generate-document-similarity-matrix ((collection document-graph))
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
