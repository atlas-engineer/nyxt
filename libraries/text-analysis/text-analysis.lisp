;;; text-analysis.lisp -- functions for facilitating text analysis

(in-package :text-analysis)

(defparameter *stop-words* nil "List of stop words to use during tokenization.")

(defun tokenize-string (string)
  (str:split " " string))

(defclass document ()
  ((string-contents :initarg :string-contents :accessor string-contents)
   (word-count :initform (make-hash-table :test #'equal) :accessor
   word-count :documentation "Contains a mapping of word -> amount of
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

(defclass document-collection ()
  ((documents :initform () :initarg :documents :accessor documents)))

(defmethod add-document ((document-collection document-collection) document)
  (push document (documents document-collection)))

(defmethod document-frequency ((document-collection document-collection) term)
  (/ (count-if (lambda (document) (> (term-frequency document term) 0)) (documents document-collection))
     (length (documents document-collection))))
