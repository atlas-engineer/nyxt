;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package nyxt/utilities
  (:use #:common-lisp)
  (:import-from #:serapeum #:export-always #:->))

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
                       ;; FUNCTION-NAME and cache it too.
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
  (open pathname :direction :probe :if-does-not-exist :create))

(export-always 'funcall*)
(defun funcall* (f &rest args)
  "Like `funcall' but does nothing when F is nil."
  (when f (apply #'funcall f args)))

(defun guess-external-format (filename)
  (or (swank-backend:guess-external-format filename)
      (swank-backend:find-external-format "latin-1")
      :default))

(export-always 'source-for-thing)
(-> source-for-thing ((or function method class)) string)
(defun source-for-thing (thing)
  "Return the string source for THING, if any.
If there's no source, returns empty string.
THING can be a class or a function, not symbol."
  (or (alex:when-let* ((full-definition (swank:find-definition-for-thing thing))
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
                       (*package* (if (and (listp name)
                                           (eq :macro (first name)))
                                      (symbol-package (second name))
                                      (symbol-package name)))
                       (file (uiop:file-exists-p (first (alexandria:assoc-value definition :file))))
                       (file-content (alexandria:read-file-into-string
                                      file
                                      :external-format (guess-external-format file)))
                       (start-position (first (alexandria:assoc-value definition :position))))
        (handler-case
            (let ((*read-eval* nil))
              (let ((expression (read-from-string file-content t nil
                                                  :start (1- start-position))))
                (values (let ((*print-case* :downcase)
                              (*print-pretty* t))
                          (write-to-string expression))
                        expression)))
          (reader-error ()
            (str:trim-right
             (subseq file-content
                     (max 0 (1- start-position))
                     (search (uiop:strcat +newline+ "(") file-content :start2 start-position))))))
      ""))

(export-always 'function-lambda-string)
(defun function-lambda-string (fun)
  "Like `function-lambda-expression' for the first value, but return a string.
On failure, fall back to other means of finding the source.
Return the lambda s-expression as a second value, if possible."
  (alex:if-let ((expression (when (functionp fun) (function-lambda-expression fun))))
    (values (let ((*print-case* :downcase)
                  (*print-pretty* t))
              (write-to-string expression))
            expression)
    (source-for-thing fun)))

(-> last-word (string) string)
(export-always 'last-word)
(defun last-word (s)
  (if (uiop:emptyp s)
      ""
      (the (values string &optional) (alex:last-elt (sera:words s)))))

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

(export-always 'reduce/append)
(defun reduce/append (list-of-lists)
  "Return the appended sublists in LIST-OF-LISTS.
Like `uiop:reduce/strcat' but for lists."
  ;; We reduce from the end stay in O(n), otherwise we'd be in O(n²).
  ;; We do not use `(mapcar #'identity ...)' which alters the sublists.
  (reduce #'append list-of-lists :from-end t))

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
