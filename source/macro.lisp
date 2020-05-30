;;; macro.lisp --- macros used in Next

(in-package :next)

(export-always 'defclass-export)
(defmacro defclass-export (name supers &body (slots . options))
  `(serapeum.exporting:defclass ,name ,supers ,slots ,@options))

;; While we don't use this macro yet, it could prove useful if we decide to use
;; a `defclass' macro other than serapeum's.
(defmacro export-class (class-symbol &optional
                                       (package nil package-supplied?)
                                       (accessor-fn #'identity))
  "Export class and all its readers, writers and accessors.
Accessor symbols are derived from ACCESSOR-FN applied to the slot name."
  (let* ((slot-definitions (closer-mop:class-direct-slots (find-class class-symbol)))
         (slot-readers (mapcar #'car (mapcar #'closer-mop:slot-definition-readers slot-definitions)))
         (slot-writers (mapcar #'cadar (mapcar #'closer-mop:slot-definition-writers slot-definitions)))
         (slot-accessors (delete-if-not (lambda (sym)
                                          (and (typep (symbol-function sym) 'generic-function)
                                               (some (lambda (method)
                                                       (equal (closer-mop:method-lambda-list method)
                                                              (list class-symbol)))
                                                     (closer-mop:generic-function-methods (symbol-function 'active-buffer)))))
                                        (mapcar accessor-fn (mapcar #'closer-mop:slot-definition-name slot-definitions))))
         (syms (delete-duplicates (append slot-readers slot-writers slot-accessors))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',class-symbol)
         (export ',syms ,@(and package-supplied? (list package)))))))

;; WARNING: Compile-time type-checking with `satisfies' only works at the
;; top-level with SBCL.
(defmacro define-class-type (class-sym)
  "Define a type named CLASS-SYM-type.
An object of this type is a subclass of CLASS-SYM."
  (let ((type-pred (intern (str:concat (string class-sym) "-TYPE-P")))
        (type-fun (intern (str:concat (string class-sym) "-TYPE"))))
    `(progn
       (defun ,type-pred (class-symbol)
         (closer-mop:subclassp (find-class class-symbol)
                               (find-class ',class-sym)))
       (deftype ,type-fun ()
         '(satisfies ,type-pred)))))

;; TODO: Remove implicit %buffer and %callback parameter.
(export-always 'define-parenscript)
(defmacro define-parenscript (script-name args &body script-body)
  "Define parenscript function SCRIPT-NAME.
SCRIPT-BODY must be a valid parenscript and will be wrapped in (PS:PS ...).
Any Lisp expression must be wrapped in (PS:LISP ...).

The returned function is called over 3 key arguments beside ARGS:
- %CALLBACK: a function to call when the script returns.  Defaults to nil.
- %BUFFER: The buffer used to execute the script.  Defaults to the current buffer.

Those variables can be used from the SCRIPT-BODY (the parenscript code).

ARGS must be key arguments."
  `(progn
     (defun ,script-name ,(append '(&key ((:callback %callback) nil)
                                    ((:buffer %buffer) (current-buffer)))
                           args)
       (ffi-buffer-evaluate-javascript %buffer
                                       (ps:ps ,@script-body)
                                       :callback %callback))))

(defmacro with-ps (&body body)
  "Execute the parenscript body against the current buffer."
  ;XXX: we might as well do it synchronously.
  `(with-result (res
                 (ffi-buffer-evaluate-javascript (current-buffer)
                                                 (ps:ps ,@body)
                                                 :callback (lambda (res)
                                                             (format t res))))
     (declare (ignorable res))))

(export-always 'with-result)
(defmacro with-result ((symbol async-form) &body body)
  "Call ASYNC-FORM.
When ASYNC-FORM returns, its result is bound to SYMBOL and BODY is executed.
ASYNC-FORM is a function that has at least a :CALLBACK key argument.

Example:

  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt \"Bookmark URL\"))
    (bookmark-add url))"
  `(,(first async-form)
    ,@(rest async-form)
    :callback (lambda (,symbol) ,@body)))

(export-always 'with-result*)
(defmacro with-result* (bindings &body body)
  "Like WITH-RESULT but allows for chained asynchronous bindings.

Example:

  (with-result* ((links-json (add-link-hints))
                 (selected-hint (read-from-minibuffer
                                 (make-minibuffer
                                  :input-prompt \"Bookmark hint\"
                                  :cleanup-function #'remove-link-hints))))
    ...)"
  (if (null bindings)
    `(progn ,@body)
    `(with-result ,(first bindings)
       (with-result* ,(rest bindings) ,@body))))

(defparameter *yes-no-choices* '("yes" "no"))

(defun yes-no-completion-filter ()
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer) *yes-no-choices*)))

(defun confirmed-p (answer)
  (string-equal answer "yes"))

(defmacro with-confirm (prompt &body body)
  "Ask the user for confirmation before executing BODY.
PROMPT is a list fed to `format nil'.

Example usage:

  (with-confirm (\"Are you sure to kill ~a buffers?\" count)
     (delete-buffers))
"
  `(with-result (answer (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt (format nil ,@prompt)
                          :completion-function (yes-no-completion-filter)
                          :show-completion-count nil)))
     (when (confirmed-p answer)
       ,@body)))
