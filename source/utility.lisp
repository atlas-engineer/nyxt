;;; utility.lisp --- utility classes and functions

(in-package :next)

(defmethod object-string ((object t))
  (princ-to-string object))

;; data node used to represent tree history
(defclass node ()
    ((parent :accessor node-parent :initarg :parent :initform nil)
     (children :accessor node-children :initform nil)
     (data :accessor node-data :initarg :data :initform nil)))

(defmethod object-string ((node node))
  (node-data node))

(define-command load-file ()
  "Load a file by specifying the absolute path to that file."
  (with-result (file-name-input (read-from-minibuffer
                                 (minibuffer *interface*)
                                 :input-prompt "Load file:"))
    (load file-name-input :if-does-not-exist nil)))

(defun reload-init ()
  (load *init-file-path*))

(define-command start-swank ()
  "Start a swank server that can be connected to in Emacs via
slime. Default port is 4006."
  (swank:create-server :port *swank-port* :dont-close t))

(defun parse-url (input-url)
  (handler-case
      ;; puri:parse-uri fails on crazy inputs like:
      ;; - hello world
      ;; - https://www.google.com/search?q=hello world
      (let ((url (puri:parse-uri input-url)))
        (cond
          ((puri:uri-scheme url) input-url)
          ((probe-file input-url)
           (concatenate 'string "file://" input-url))
          (t (generate-search-query input-url))))
    (puri:uri-parse-error ()
      input-url)))

(defun generate-search-query (search-string)
  (let* ((encoded-search-string
           (cl-string-match:replace-re "  *" "+" search-string :all t))
         (url (concatenate 'string "https://duckduckgo.com/?q=" encoded-search-string)))
    url))

(defun fuzzy-match (input candidates &key (accessor-function nil)
                                          (case-sensitive nil))
  "fuzzy-match works by taking a string input from the user. The
string is then populated with '*' between each character to create a
regex. As an example, 'nt' will become 'n.*t.*'. This will enable
matching of 'next' or 'note', etc. This function currently limits the
type of input that it accepts, only matching against alphabetical input. An
advanced version of this command may allow for complete regular expressions, but
will have to consider malformed ones."
  (let* ((cleaned-input (cl-string-match:replace-re
                         "[^a-zA-Z]" "" input :all t))
         (cleaned-input (if case-sensitive
                            cleaned-input
                            (string-downcase cleaned-input)))
         (regex
           (with-output-to-string (stream)
             (loop for char across cleaned-input do
               (princ #\. stream)
               (princ #\* stream)
               (princ char stream))
             ;; match any chars after final char in cleaned-input
             (princ #\. stream)
             (princ #\* stream)))
         (completions nil))
    ;; use constructed regex to see which options match
    (flet ((candidate-representation (candidate)
             (if accessor-function
                 (funcall accessor-function candidate) candidate)))
      (loop for candidate in candidates do
        (when (cl-string-match:match-re
               regex
               (if case-sensitive
                   (candidate-representation candidate)
                   (string-downcase (candidate-representation candidate))))
          (push candidate completions))))
    completions))

;; utility functions for getting paths from the xdg directory
;; specification in a namespaced directory for Next
(defun xdg-data-home (&optional (file-name ""))
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-data-home))))

(defun xdg-config-home (&optional (file-name ""))
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-config-home))))

(defun ensure-parent-exists (path)
  "Create parent directories of PATH if they don't exist and return PATH."
  (ensure-directories-exist (directory-namestring path))
  path)

(defun ensure-file-exists (path &optional (init-function))
  "Create file pointed by PATH if it does not exists.  Return PATH's truename.
When non-nil, INIT-FUNCTION is used to create the file, else the file will be empty."
  (unless (probe-file path)
    (if init-function
        (funcall init-function path)
        (close (open (ensure-parent-exists path) :direction :probe :if-does-not-exist :create))))
  (truename path))

(defun set-default (class-name slot-name value)
  "Set default value of slot SLOT-NAME from class CLASS-NAME.
Return the class."
  ;; Warning: This is quite subtle: the :initform and :initfunction are tightly
  ;; coupled, it seems that both must be changed together.  We need to change
  ;; the class-slots and not the class-direct-slots.  TODO: Explain why.
  (let* ((class (closer-mop:ensure-finalized (find-class class-name)))
         (slot (find-if (lambda (slot)
                         (eq (closer-mop:slot-definition-name slot)
                             slot-name))
                       (closer-mop:class-slots class))))
    (setf
     (closer-mop:slot-definition-initform slot) value
     (closer-mop:slot-definition-initfunction slot) (lambda () value))
    class))
