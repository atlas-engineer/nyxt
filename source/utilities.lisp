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

(export-always 'defmemo)
(defmacro defmemo (name params &body body) ; TODO: Replace with https://github.com/AccelerationNet/function-cache?
  (multiple-value-bind (required optional rest keyword)
      (alex:parse-ordinary-lambda-list params)
    (alex:with-gensyms (memo-table args)
      `(let ((,memo-table (make-hash-table :test 'equal)))
         (defun ,name (,@params)
           (let ((,args (append (list ,@required)
                                (list ,@optional)
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
    (or (alex:when-let* ((full-definition (swank:find-definition-for-thing fun))
                         (definition (and (not (eq :error (first full-definition)))
                                          (rest full-definition)))
                         (*package* (symbol-package (swank-backend:function-name
                                                     (if (functionp fun)
                                                         fun
                                                         (closer-mop:method-generic-function fun)))))
                         (file (uiop:file-exists-p (first (alexandria:assoc-value definition :file))))
                         (file-content (alexandria:read-file-into-string
                                        file
                                        :external-format (guess-external-format file)))
                         (start-position (first (alexandria:assoc-value definition :position))))
          (restart-case
              (handler-bind ((reader-error (lambda (c)
                                             (declare (ignore c))
                                             (invoke-restart 'use-value
                                                             (str:trim-right
                                                              (subseq file-content
                                                                      (max 0 (1- start-position))
                                                                      (search (uiop:strcat +newline+ "(") file-content :start2 start-position)))))))
                (let ((*read-eval* nil))
                  (let ((expression (read-from-string file-content t nil
                                                      :start (1- start-position))))
                    (values (let ((*print-case* :downcase)
                                  (*print-pretty* t))
                              (write-to-string expression))
                            expression))))
            (use-value (arg)
              arg)))
        "")))

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

(export-always 'on)
(defmacro on (hook args &body body)
  "Attach a handler with ARGS and BODY to the HOOK.

ARGS can be
- A symbol if there's only one argument to the callback.
- A list of arguments.
- An empty list, if the hook handlers take no argument."
  (let ((handler-name (gensym "on-hook-handler"))
        (args (alex:ensure-list args)))
    `(nhooks:add-hook
      ,hook (make-instance 'nhooks:handler
                           :fn (lambda ,args
                                 (declare (ignorable ,@args))
                                 ,@body)
                           :name (quote ,handler-name)))))

(export-always 'once-on)
(defmacro once-on (hook args &body body)
  "Attach a handler with ARGS and BODY to the HOOK.

Remove the handler after it fires the first time.

See `on'."
  (let ((handler-name (gensym "once-on-hook-handler"))
        (args (alex:ensure-list args)))
    (alex:once-only (hook)
      `(nhooks:add-hook
        ,hook (make-instance 'nhooks:handler
                             :fn (lambda ,args
                                   (declare (ignorable ,@args))
                                   (nhooks:remove-hook ,hook (quote ,handler-name))
                                   ,@body)
                             :name (quote ,handler-name))))))

(export-always 'public-initargs)
(defun public-initargs (class-specifier)
  "Return the list of initargs for CLASS-SPECIFIER direct slots."
  (remove-if (lambda (name)
               (eq :internal
                   (nth-value 1 (find-symbol (string name) (symbol-package name)))))
             (mopu:direct-slot-names class-specifier)))

(export-always 'read*)
(defun read* (&optional
                (input-stream *standard-input*)
                (eof-error-p t)
                (eof-value nil)
                (recursive-p nil))
  "Like `read' with standard IO syntax but does not accept reader macros ('#.').
This is useful if you do not trust the input."
  (let ((old-package *package*))
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* old-package))
        (read input-stream eof-error-p eof-value recursive-p)))))

(export-always 'read-from-stream)
(defun read-from-stream (stream)
  "Return a list of all s-expressions read in STREAM."
  (loop :for value := (read* stream nil stream)
        :until (eq value stream)
        :collect value))

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
