(in-package :text-analysis)

;;; text-analysis.lisp -- functions for facilitating text analysis

(defun word-tokenize (string &key (remove-stop-words t) (stem nil) (down-case t) (alphabeticp t))
  "Split a string into a list of words."
  (let* ((alpha-scanner (cl-ppcre:create-scanner "^[A-Za-z]*$"))
         (tokens (str:split " " (str:collapse-whitespaces string)))
         (tokens (if remove-stop-words
                     (delete-if (lambda (x) (gethash (string-downcase  x) (stop-words-lookup *language-data*))) tokens)
                     tokens))
         (tokens (if stem
                     (mapcar #'stem tokens)
                     tokens))
         (tokens (if down-case
                     (mapcar #'string-downcase tokens)
                     tokens))
         (tokens (if alphabeticp
                     (delete-if-not (lambda (x) (cl-ppcre:scan alpha-scanner x)) tokens)
                     tokens)))
    tokens))

(defclass document ()
  ((string-contents :initarg :string-contents :accessor string-contents)
   (word-count :initform (make-hash-table :test #'equal)
               :accessor word-count
               :documentation "Contains a mapping of word -> amount of
   times word appears in the document.")
   (vector-form :accessor vector-form
                :documentation "Vector representation of the document.")
   (rank :accessor rank :documentation "Rank used for sorting.")
   (tokens :accessor tokens)
   (token-count :accessor token-count)))

(defmethod initialize-instance :after ((document document) &key)
  (setf (tokens document) (word-tokenize (string-contents document)))
  (setf (token-count document) (length (tokens document)))
  (loop for token in (tokens document) do
    (incf (gethash token (word-count document) 0))))

(defmethod term-frequency ((document document) term)
  (/ (gethash term (word-count document) 0) (token-count document)))

(defmethod termp ((document document) term)
  "Does the term exist in the document?"
  (> (gethash term (word-count document) 0) 0))

(defmethod term-count ((document document) term)
  (gethash term (word-count document) 0))

(defclass document-collection ()
  ((documents :initform () :initarg :documents :accessor documents)))

(defmethod add-document ((document-collection document-collection) document)
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

(defmethod dictionary ((document-collection document-collection))
  "Return a list of all of the words that appear in a document collection."
  (let ((words (list)))
    (loop for document in (documents document-collection)
          do (setf words (append words (tokens document))))
    (remove-duplicates words :test #'equalp)))

(defmethod keywords ((document document) (document-collection document-collection))
  (sort (loop for word being the hash-keys of (word-count document)
              collect (cons word (term-frequency-inverse-document-frequency
                                  document document-collection word))) #'> :key #'cdr))

(defmethod document-keywords ((document document))
  (sort (loop for word being the hash-keys of (word-count document)
              collect (cons word (term-frequency document word))) #'> :key #'cdr))
