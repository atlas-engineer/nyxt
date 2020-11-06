;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :text-analysis)

(defclass document ()
  ((string-contents :initarg :string-contents :accessor string-contents)
   (term-count-table :initform (make-hash-table :test #'equal)
                     :documentation "Contains a mapping of term -> amount of
   times word appears in the document.")
   (vector-form :accessor vector-form
                :documentation "Vector representation of the document.")
   (rank :accessor rank :documentation "Rank used for sorting.")
   (tokens :accessor tokens)
   (token-count :accessor token-count)))

(defclass document-collection ()
  ((documents :initform () :initarg :documents :accessor documents)))

(defmethod initialize-instance :after ((document document) &key)
  (setf (tokens document) (word-tokenize (string-contents document)))
  (setf (token-count document) (length (tokens document)))
  (loop for token in (tokens document) do
    (incf (gethash token (slot-value document 'term-count-table) 0))))

(defmethod term-count ((document document) term)
  (gethash term (slot-value document 'term-count-table) 0))

(defmethod term-frequency ((document document) term)
  "How often does the word exist in the document?"
  (/ (term-count document term) (token-count document)))

(defmethod termp ((document document) term)
  "Does the term exist in the document?"
  (> (term-count document term) 0))

(defmethod add-document ((document-collection document-collection) document)
  "Add a document to the document collection."
  (push document (documents document-collection)))

(defmethod document-frequency ((document-collection document-collection) term)
  (/ (count-if (lambda (document) (termp document term)) (documents document-collection))
     (length (documents document-collection))))

(defmethod inverse-document-frequency ((document-collection document-collection) term)
  (log (/ (length (documents document-collection))
          (count-if (lambda (document) (termp document term)) (documents document-collection)))))

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
          do (setf words (append words (tokens document))))
    (remove-duplicates words :test #'equalp)))

(defmethod keywords ((document document) (document-collection document-collection))
  (sort (loop for word in (dictionary document)
              collect (cons word (term-frequency-inverse-document-frequency
                                  document document-collection word))) #'> :key #'cdr))

(defmethod document-keywords ((document document))
  (sort (loop for word in (dictionary document)
              collect (cons word (term-frequency document word))) #'> :key #'cdr))
