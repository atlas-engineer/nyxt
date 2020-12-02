;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

;;; document-vector.lisp: transform a document into a vector

(defmethod word-count-vectorize ((document document) dictionary)
  "Transform a document into a vector using word counts."
  (let ((vector-data (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-data)
          do (setf (aref vector-data index) (term-count document word)))
    (setf (vector-data document) vector-data)))

(defmethod tf-idf-vectorize ((document document) (collection document-collection) dictionary)
  "Transform a document into a vector using tf-idf."
  (let ((vector-data (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-data)
          do (setf (aref vector-data index)
                   (term-frequency-inverse-document-frequency document collection word)))
    (setf (vector-data document) vector-data)))

(defmethod tf-vectorize ((document document) dictionary)
  "Transform a document into a vector using tf."
  (let ((vector-data (make-array (length dictionary) :initial-element 0)))
    (loop for word in dictionary
          for index from 0 below (length vector-data)
          do (setf (aref vector-data index)
                   (term-frequency document word)))
    (setf (vector-data document) vector-data)))

(defmethod vectorize-documents ((document-collection document-collection) operation)
  (let ((dictionary (dictionary document-collection)))
    (loop for document in (documents document-collection)
          do (funcall operation :document document
                                :dictionary dictionary))))

(defmethod word-count-vectorize-documents ((document-collection document-collection))
  (vectorize-documents document-collection (lambda (&key document dictionary)
                                             (word-count-vectorize document dictionary))))

(defmethod tf-vectorize-documents ((document-collection document-collection))
  (vectorize-documents document-collection (lambda (&key document dictionary)
                                             (tf-vectorize document dictionary))))

(defmethod tf-idf-vectorize-documents ((document-collection document-collection))
  (vectorize-documents document-collection (lambda (&key document dictionary)
                                             (tf-idf-vectorize document document-collection dictionary))))


