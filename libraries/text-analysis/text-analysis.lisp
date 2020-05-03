;;; text-analysis.lisp -- functions for facilitating text analysis

(in-package :text-analysis)

(defun tokenize-string (string &key (remove-stop-words t))
  (let* ((tokens (str:split " " string))
         (tokens (if remove-stop-words
                     (delete-if (lambda (x) (gethash x (stop-words-lookup *language-data*))) tokens)
                     tokens)))
    tokens))

(defclass document ()
  ((string-contents :initarg :string-contents :accessor string-contents)
   (word-count :initform (make-hash-table :test #'equal)
               :accessor word-count
               :documentation "Contains a mapping of word -> amount of
   times word appears in the document.")
   (tokens :accessor tokens)
   (token-count :accessor token-count)))

(defmethod initialize-instance :after ((document document) &key)
  (setf (tokens document) (tokenize-string (string-contents document)))
  (setf (token-count document) (length (tokens document)))
  (loop for token in (tokens document) do
    (incf (gethash token (word-count document) 0))))

(defmethod term-frequency ((document document) term)
  (/ (gethash term (word-count document) 0) (token-count document)))

(defmethod termp ((document document) term)
  "Does the term exist in the document?"
  (> (term-frequency document term) 0))

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

(defmethod keywords ((document document) (document-collection document-collection))
  (sort (loop for word being the hash-keys of (word-count document)
              collect (cons word (term-frequency-inverse-document-frequency
                                  document document-collection word))) #'> :key #'cdr))
