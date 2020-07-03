(in-package :text-analysis)

;;; text-rank.lisp -- implementation of textrank algorithm

(defun sentence-tokenize (string)
  "Split a string into a list of sentences."
  (mapcar #'str:trim (cl-ppcre:split "\\.|\\?|\\!" string)))

(defclass sentence (document)
  ((vector-form :accessor vector-form
                :documentation "Vector representation of the sentence.")))

(defmethod word-count-vectorize ((sentence sentence) dictionary)
  "Transform a sentence into a vector using word counts."
  (let ((vector-form (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-form)
          do (setf (aref vector-form index) (term-count sentence word)))
    (setf (vector-form sentence) vector-form)))

(defmethod word-count-vectorize-documents ((document-collection document-collection))
  (let ((dictionary (dictionary document-collection)))
    (loop for document in (documents document-collection)
          do (word-count-vectorize document dictionary))))

(defmethod cosine-similarity ((sentence-a sentence) (sentence-b sentence))
  (flet ((vector-product (sentence-a sentence-b)
           (loop for a across (vector-form sentence-a)
                 for b across (vector-form sentence-b)
                 sum (* a b)))
         (vector-sum-root (sentence)
           (sqrt (loop for i across (vector-form sentence)
                       sum (* i i)))))
    (/ (vector-product sentence-a sentence-b)
       (* (vector-sum-root sentence-a) (vector-sum-root sentence-b)))))
