;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :analysis)

(export-always 'document)
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

(defclass term-statistics ()
  ((occurrences
    :initform 0
    :accessor occurrences
    :documentation "How many times the stemmed term appears.")
   (term-position
    :initarg :term-position
    :reader term-position
    :documentation "Index of the first occurrence in the document's tokens.")
   (surfaces
    :initform (make-hash-table :test #'equal)
    :accessor surfaces
    :documentation "A map from a surface form to the number of times it appeared."))
  (:documentation "Counts for one stemmed term and the surface forms that produced it."))

(-> preferred-surface (term-statistics) (or null string))
(defun preferred-surface (stat)
  "Return the surface form to show for STAT.
The most frequent form wins. A tie goes to the shorter form, then to alphabetical order."
  (let ((best nil)
        (best-count -1))
    (maphash (lambda (word count)
               (when (or (> count best-count)
                         (and (= count best-count)
                              (or (null best)
                                  (< (length word) (length best))
                                  (and (= (length word) (length best))
                                       (string< word best)))))
                 (setf best word
                       best-count count)))
             (surfaces stat))
    best))

(-> term-table (list) hash-table)
(defun term-table (tokens)
  "Map each stemmed token to its `term-statistics'.
Tokens shorter than two characters are ignored."
  (let ((table (make-hash-table :test #'equal)))
    (loop for token in tokens
          for position from 0
          unless (< (length token) 2)
            do (let* ((stem (stem-word token))
                      (stat (or (gethash stem table)
                                (setf (gethash stem table)
                                      (make-instance 'term-statistics
                                                     :term-position position)))))
                 (incf (occurrences stat))
                 (incf (gethash token (surfaces stat) 0))))
    table))

(-> keyword-entry-greater-p (list list) boolean)
(defun keyword-entry-greater-p (left right)
  "True when LEFT should rank above RIGHT.
Entries are (surface score position). Higher scores win, then earlier position,
then alphabetical order."
  (cond ((> (second left) (second right)) t)
        ((< (second left) (second right)) nil)
        ((< (third left) (third right)) t)
        ((> (third left) (third right)) nil)
        (t (string< (first left) (first right)))))

(-> top-keywords (hash-table list integer &optional (or null integer)) list)
(defun top-keywords (table tables document-count &optional limit)
  "Return an alist of (surface . score) for TABLE.
Score is the term count times a smoothed inverse document frequency across
TABLES. DOCUMENT-COUNT is the number of documents in that collection."
  (let ((entries '())
        (document-count (max 1 document-count)))
    (maphash (lambda (stem stat)
               (let* ((frequency (count-if (lambda (other) (gethash stem other)) tables))
                      (idf (log (+ 1 (/ document-count (max 1 frequency)))))
                      (score (* (occurrences stat) idf)))
                 (push (list (preferred-surface stat) score (term-position stat))
                       entries)))
             table)
    (let ((sorted (sort entries #'keyword-entry-greater-p)))
      (mapcar (lambda (entry) (cons (first entry) (second entry)))
              (if limit
                  (serapeum:take limit sorted)
                  sorted)))))

(-> rank-keywords (list &key (:limit (or null integer))) list)
(defun rank-keywords (texts &key limit)
  "Return one keyword alist per string in TEXTS.
Terms are scored across TEXTS, so a term that occurs in many of them sinks."
  (if (endp texts)
      '()
      (let* ((documents (mapcar (lambda (text)
                                  (make-instance 'document
                                                 :string-contents (if (stringp text) text "")))
                                texts))
             (tables (mapcar (lambda (document) (term-table (tokens document)))
                             documents)))
        (mapcar (lambda (table)
                  (top-keywords table tables (length tables) limit))
                tables))))

(export-always 'keywords)
(defmethod keywords ((document document) &optional document-collection)
  "Return an alist of (term . score), highest score first.
A term that occurs often in DOCUMENT scores higher. When DOCUMENT-COLLECTION
is supplied, a term that also occurs in the other documents scores lower.
Equal scores break by earlier occurrence, then by the term itself, so the
same document always yields the same keywords."
  (let* ((documents (if document-collection
                        (or (documents document-collection) (list document))
                        (list document)))
         (tables (mapcar (lambda (doc) (term-table (tokens doc))) documents))
         (index (position document documents))
         (table (if index
                    (elt tables index)
                    (term-table (tokens document)))))
    (top-keywords table tables (length tables))))

(export-always 'extract-keywords)
(-> extract-keywords ((or string list) &key (:limit integer)) list)
(defun extract-keywords (text &key (limit 5))
  "Return up to LIMIT keywords for TEXT.
TEXT is a string or a list of strings. A string yields one alist of
(term . score). A list yields one such alist per string, scored against the
other strings so a term that occurs in many of them sinks. Word forms share
a stem, and the result is stable for a given text."
  (etypecase text
    (string (first (rank-keywords (list text) :limit limit)))
    (list (rank-keywords text :limit limit))))
