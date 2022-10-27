;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

(defclass document ()
  ((source :accessor source :initarg :source
           :documentation "The source object for the document.")
   (string-contents :initarg :string-contents :accessor string-contents)
   (term-count-table :initform (make-hash-table :test #'equal)
                     :documentation "Contains a mapping of term ->
amount of times word appears in the document.")
   (vector-data :accessor vector-data
                :documentation "Vector representation of the document.")
   (rank :accessor rank :documentation "Rank used for sorting.")
   (tokens :accessor tokens)
   (token-count :accessor token-count))
  (:documentation "The document class represents a document. After
creating a document, you can perform several operations on it, some
examples:

+ term count: how many times does a term appear in a document?
+ term frequency: how many times does a term appear divided by the
  total number of words in the document?"))

(defclass document-collection ()
  ((documents :initform () :initarg :documents :accessor documents))
  (:documentation "The document collection class represents a
collection of documents. As with a document, there are several
operations available, some examples:

+ dictionary: which words appear in the document collection?
+ keywords: what are the important keywords in this document
  collection?"))

(defmethod initialize-instance :after ((document document) &key)
  (setf (tokens document) (word-tokenize (string-contents document)))
  (setf (token-count document) (length (tokens document)))
  (loop for token in (tokens document) do
    (incf (gethash token (slot-value document 'term-count-table) 0))))

(defmethod term-count ((document document) term)
  (gethash term (slot-value document 'term-count-table) 0))

(defmethod term-frequency ((document document) term)
  "How often does the word exist in the document?"
  (/ (term-count document term)
     ;; prevent division by zero for malformed documents
     (max 1 (token-count document))))

(defmethod termp ((document document) term)
  "Does the term exist in the document?"
  (> (term-count document term) 0))

(defmethod add-document ((document-collection document-collection) document)
  "Add a document to the document collection."
  (push document (documents document-collection)))

(defun match-term (term)
  (lambda (document)
    (termp document term)))

(defmethod document-frequency ((document-collection document-collection) term)
  (/ (count-if (match-term term) (documents document-collection))
     (length (documents document-collection))))

(defmethod inverse-document-frequency ((document-collection document-collection) term)
  (log (/ (length (documents document-collection))
          (count-if (match-term term) (documents document-collection)))))

(defmethod term-frequency-inverse-document-frequency ((document document)
                                                      (document-collection document-collection)
                                                      term)
  (* (term-frequency document term) (inverse-document-frequency document-collection term)))

(defmethod dictionary ((document document))
  "Return a list of all of the words that appear in a document."
  (loop for key being the hash-keys of (slot-value document 'term-count-table)
        collect key))

(defmethod dictionary ((document-collection document-collection))
  "Return a list of all of the words that appear in a document collection."
  (let ((words (list)))
    (loop for document in (documents document-collection)
          do (alexandria:appendf words (tokens document)))
    (remove-duplicates words :test #'equalp)))

(defmethod keywords ((document document) &optional document-collection)
  (if document-collection
      (sort (loop for word in (dictionary document)
                  collect (cons word (term-frequency-inverse-document-frequency
                                      document document-collection word)))
            #'>
            :key #'cdr)
      (sort (loop for word in (dictionary document)
                  collect (cons word (term-frequency document word)))
            #'>
            :key #'cdr)))

(defun extract-keywords (text &key (limit 5))
  "Extract keywords from a string of text."
  (serapeum:take limit (keywords (make-instance 'analysis:document
                                                :string-contents text))))
