(in-package :nyxt)

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

(defparameter %callback nil)            ; TODO: Make a monad?

(export-always 'define-parenscript)
(defmacro define-parenscript (script-name args &body script-body)
  "Define parenscript function SCRIPT-NAME.
SCRIPT-BODY must be a valid parenscript and will be wrapped in (PS:PS ...).
Any Lisp expression must be wrapped in (PS:LISP ...).

The returned function is called with the ARGS lambda-list over the current
buffer."
  `(progn
     (defun ,script-name ,args
       (ffi-buffer-evaluate-javascript (current-buffer)
                                       (ps:ps ,@script-body)))))

(export-always 'pflet)
(defmacro pflet (((function function-arguments &body function-body)) &body body)
  "Define single parenscript function in a flet body."
  `(flet ((,function ,function-arguments
            (ffi-buffer-evaluate-javascript (current-buffer)
                                            (ps:ps ,@function-body))))
     ,@body))

(export-always 'use-empty-result)
(defun use-empty-result ()
  (funcall-safely %callback nil))

(export-always 'with-result)
(defmacro with-result ((symbol async-form) &body body)
  "Call ASYNC-FORM and lexically bind `%callback' to BODY.
SYMBOL is bound to the result of ASYNC-FORM in the lexical context of BODY.

If ASYNC-FORM does not funcall `%callback', BODY won't be called.
Call `use-empty-result' without argument in ASYNC-FORM to force a call to BODY.

Example:

  (with-result (url (read-from-minibuffer
                     (make-minibuffer
                      :input-prompt \"Bookmark URL\"))
    (bookmark-add url))"
  `(let ((%callback (lambda (,symbol) ,@body)))
     ,async-form))

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

(export-always '*yes-no-choices*)
(defparameter *yes-no-choices* '(:yes "yes" :no "no")
  "The suggestions when asking the user for a yes/no choice.
See `with-confirm'.
The first suggestion poses as a default.")

(defun yes-no-suggestion-filter ()
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer) (sera:plist-values *yes-no-choices*))))

(defun confirmed-p (answer)
  (string-equal answer (getf *yes-no-choices* :yes)))

(export-always 'with-confirm)
(defmacro with-confirm (prompt &body body)
  "Ask the user for confirmation before executing BODY.
PROMPT is a list fed to `format nil'.

Example usage defaulting to \"no\":

\(let ((*yes-no-choices* '(:no \"no\" :yes \"yes\")))
  (with-confirm (\"Are you sure to kill ~a buffers?\" count)
     (delete-buffers)))"
  `(with-result (answer (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt (format nil ,@prompt)
                          :suggestion-function (yes-no-suggestion-filter)
                          :hide-suggestion-count-p t)))
     (when (confirmed-p answer)
       ,@body)))
