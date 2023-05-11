;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/utilities
  (:use :cl)
  (:import-from :serapeum #:export-always #:->))

(in-package :nyxt/utilities)
(serapeum:eval-always
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria-2 :nyxt/utilities)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum))

;; Ensure the top-level exported forms are alphabetically sorted.

(export-always '+newline+)
(alex:define-constant +newline+ (string #\newline) :test #'equal)

(export-always '+escape+)
(alex:define-constant +escape+ (string #\escape) :test #'equal)

(export-always 'new-id)
(defun new-id ()
  "Generate a new unique numeric ID."
  (parse-integer (symbol-name (gensym ""))))

(export-always 'defmemo)
(defmacro defmemo (name params &body body) ; TODO: Replace with https://github.com/AccelerationNet/function-cache?
  "Define a new memoized function named NAME in the global environment.

Functionally equivalent to `defun' but the function stores its
computations, and remembers its passed parameters when invoked so that
any expensive computation only takes place once."
  ;; Parse the functions' arguments and store the parameters in a hash table
  (multiple-value-bind (required optional rest keyword)
      (alex:parse-ordinary-lambda-list params)
    (alex:with-gensyms (memo-table args)
      `(let ((,memo-table (make-hash-table :test 'equal)))
         (defun ,name (,@params)
           (let ((,args (append (list ,@required)
                                (list ,@(mapcar #'first optional))
                                ,rest
                                (list ,@(alex:mappend #'first keyword)))))
             (alex:ensure-gethash
              ,args ,memo-table
              (apply (lambda ,params
                       ;; This block is here to catch the return-from
                       ;; NAME and cache it too.
                       ;;
                       ;; TODO: Better way? Maybe use methods and
                       ;; :around qualifiers?
                       (block ,name ,@body))
                     ,args))))))))

(export-always 'destroy-thread*)
(defun destroy-thread* (thread)
  "Like `bt:destroy-thread' but does not raise an error.
Particularly useful to avoid errors on already terminated threads."
  (ignore-errors (bt:destroy-thread thread)))

(export-always 'ensure-file-exists)
(defun ensure-file-exists (pathname)
  "Create file PATHNAME unless it exists."
  (open pathname :direction :probe :if-does-not-exist :create))

(export-always 'funcall*)
(defun funcall* (f &rest args)
  "Like `funcall' but does nothing when F is nil."
  (when f (apply #'funcall f args)))

(defun guess-external-format (filename)
  (or (swank-backend:guess-external-format filename)
      (swank-backend:find-external-format "latin-1")
      :default))

(export-always 'prini)
(defun prini (value stream &rest keys &key (case :downcase) (pretty t)  (circle nil)
                                        (readably nil) (package *package*) &allow-other-keys)
  "PRINt for Interface: a printing primitive with the best aesthetics for Nyxt interfaces.
`write'-s the VALUE to STREAM with CASE, PRETTY, CIRCLE, and READABLY set to the
most intuitive values."
  (let ((*print-case* case)
        (*print-pretty* pretty)
        (*print-circle* circle)
        (*print-readably* readably)
        (*package* (find-package package)))
    (remf keys :package)
    (apply #'write value :stream stream keys)))

(export-always 'prini-to-string)
(defun prini-to-string (value &rest keys &key (case :downcase) (pretty t) (circle nil)
                                           (readably nil) (package *package*) &allow-other-keys)
  "A string-returning version of `prini'."
  (declare (ignorable case pretty circle readably package))
  (with-output-to-string (s)
    (apply #'prini value s keys)))

(export-always 'source-for-thing)
(-> source-for-thing ((or function method class)) *)
(defun source-for-thing (thing)
  "Return
- the string source for THING, if any,
- the s-expression for THING, if parseable,
- the file source belongs to.

If there's no source, return empty string.
THING can be a class or a function, not symbol."
  (sera:and-let* ((full-definition (swank:find-definition-for-thing thing))
                  (definition (and (not (eq :error (first full-definition)))
                                   (rest full-definition)))
                  ;; REVIEW: Returns (:macro name) for macros on
                  ;; SBCL. What does it do on CCL, ECL etc?
                  (name (typecase thing
                          ;; REVIEW: How do we handle macros here?
                          (class (class-name thing))
                          (method (swank-backend:function-name
                                   (closer-mop:method-generic-function thing)))
                          (function (swank-backend:function-name thing))))
                  ;; This one is to clean up the (:macro name) form.
                  (name (if (and (listp name)
                                 (member (first name) '(:special :macro)))
                            (second name)
                            name))
                  (*package* (symbol-package name))
                  ;; `swank:find-definition-for-thing' returns nonsense for
                  ;; macros, need to use `swank-backend:find-definitions'
                  ;; for those instead.
                  (definition (if (macro-function name)
                                  (or (cdadar (swank-backend:find-definitions name))
                                      definition)
                                  definition))
                  (file (uiop:file-exists-p (first (alexandria:assoc-value definition :file))))
                  (file-content (alexandria:read-file-into-string
                                 file
                                 :external-format (guess-external-format file)))
                  (start-position (first (alexandria:assoc-value definition :position))))
    (handler-case
        (let ((*read-eval* nil))
          (let ((expression (read-from-string file-content t nil
                                              :start (1- start-position))))
            (values (prini-to-string expression)
                    expression
                    file)))
      (reader-error ()
        (str:trim-right
         (values (subseq file-content
                         (max 0 (1- start-position))
                         (search (uiop:strcat +newline+ "(") file-content :start2 start-position))
                 nil
                 file))))))

(export-always 'function-lambda-string)
(defun function-lambda-string (fun)
  "Like `function-lambda-expression' for the first value, but return a string.
On failure, fall back to other means of finding the source.
Return the lambda s-expression as a second value, if possible."
  (let ((expression (when (functionp fun) (function-lambda-expression fun))))
    (if expression
        (values (prini-to-string expression)
                expression)
        (source-for-thing fun))))

(-> documentation-line (t &optional symbol t)
    t)
(export-always 'documentation-line)
(defun documentation-line (object &optional (type t) default)
  "Return the first line of OBJECT `documentation' with TYPE.
If there's no documentation, return DEFAULT."
  (or (first (sera:lines (documentation object type) :count 1))
      default))

(-> last-word (string) string)
(export-always 'last-word)
(defun last-word (s)
  "Last substring of alphanumeric characters, or empty if none."
  (let ((words (sera:words s)))
    (the (values string &optional)
         (if words (alex:last-elt words) ""))))

(export-always 'make-ring)
(defun make-ring (&key (size 1000))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(export-always 'public-initargs)
(defun public-initargs (class-specifier)
  "Return the list of initargs for CLASS-SPECIFIER direct slots."
  (remove-if (lambda (name)
               (eq :internal
                   (nth-value 1 (find-symbol (string name) (symbol-package name)))))
             (mopu:direct-slot-names class-specifier)))

(export-always 'safe-read)
(defun safe-read (&optional
                    (input-stream *standard-input*)
                    (eof-error-p t)
                    (eof-value nil)
                    (recursive-p nil))
  "Like `read' with standard IO syntax but does not accept reader macros ('#.').
UIOP has `uiop:safe-read-from-string' but no `read' equivalent.
This is useful if you do not trust the input."
  (let ((package *package*))
    (uiop:with-safe-io-syntax (:package package)
      (read input-stream eof-error-p eof-value recursive-p))))

(export-always 'safe-sort)
(defun safe-sort (s &key (predicate #'string-lessp) (key #'string))
  "Sort sequence S of objects by KEY using PREDICATE."
  (sort (copy-seq s) predicate :key key))

(export-always 'safe-slurp-stream-forms)
(defun safe-slurp-stream-forms (stream)
  "Like `uiop:slurp-stream-forms' but wrapped in `uiop:with-safe-io-syntax' and
package set to current package."
  (let ((package *package*))
    (uiop:with-safe-io-syntax (:package package)
      (uiop:slurp-stream-forms stream))))

(export-always 'socket-p)
(defun socket-p (path)
  "Return non-nil if a PATH is a socket."
  (and (uiop:file-exists-p path)
       #+darwin
       (equal "=" (uiop:run-program (list "stat" "-f" "%T" path)
                                    :output '(:string :stripped t)))
       #+(and (not darwin) (not sbcl))
       (eq :socket (osicat:file-kind path))
       #+(and (not darwin) sbcl)
       (flet ((socket-p (path)
                (let ((socket-mask 49152)
                      (mode-mask 61440))
                  (= socket-mask
                     (logand mode-mask
                             (sb-posix:stat-mode (sb-posix:stat path)))))))
         (socket-p path))))

(export-always 'has-method-p)
(defun has-method-p (object generic-function)
  "Return non-nil if OBJECT has GENERIC-FUNCTION specialization."
  (some (lambda (method)
          (subtypep (type-of object) (class-name
                                      (first (closer-mop:method-specializers method)))))
        (closer-mop:generic-function-methods generic-function)))

(export-always 'smart-case-test)
(-> smart-case-test (string) function)
(defun smart-case-test (string)
  (if (str:downcasep string) #'string-equal #'string=))
