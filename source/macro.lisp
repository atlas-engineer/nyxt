(in-package :nyxt)

(defmacro replace-class (from to)
  "Set class corresponding to FROM symbol to that of TO.
Class TO is untouched.
FROM and TO are unquoted symbols.
Return new class.

This macro ensures the type corresponding to the FROM symbol maps the TO class.
Otherwise the old type could be undefined, as is the case with CCL (SBCL
maintains the type though).  See the `find-class' documentation in the
HyperSpec:

  The results are undefined if the user attempts to change or remove the class
  associated with a symbol that is defined as a type specifier in the standard."
  `(progn
     (setf (find-class ',from) (find-class ',to))
     ;; TODO: Test with implementations beyond SBCL and CCL.
     #-SBCL
     (deftype ,from () ',to)
     (find-class ',from)))

(export-always 'defclass-export)
(defmacro defclass-export (name supers &body (slots . options))
  "Define and export class.
Additionally, this class definition macro supports cycle in the superclasses,
e.g.  (defclass-export foo (foo) ()) works."
  (if (and (find-class name nil)
           (member (find-class name)
                   (append (mapcar #'find-class supers)
                           (mapcar #'mopu:superclasses supers))))
      (let ((temp-name (gensym (string name))))
        ;; Don't export the class again.
        `(progn (defclass ,temp-name ,supers ,slots ,@options)
                (replace-class ,name ,temp-name)))
      `(serapeum.exporting:defclass ,name ,supers ,slots ,@options)))

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

(defparameter %callback nil)            ; TODO: Make a monad?

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
See `if-confirm'.
The first suggestion poses as a default.")

(defun yes-no-suggestion-filter ()
  (lambda (minibuffer)
    (fuzzy-match (input-buffer minibuffer) (sera:plist-values *yes-no-choices*))))

(defun confirmed-p (answer)
  (string-equal answer (getf *yes-no-choices* :yes)))

(export-always 'if-confirm)
(defmacro if-confirm (prompt yes-form &optional no-form)
  "Ask the user for confirmation before executing either YES-FORM of NO-FORM.
YES-FORM is executed on  \"yes\" answer, NO-FORM -- on \"no\".
PROMPT is a list fed to `format nil'.

Example usage defaulting to \"no\":

\(let ((*yes-no-choices* '(:no \"no\" :yes \"yes\")))
  (if-confirm (\"Are you sure to kill ~a buffers?\" count)
     (delete-buffers)))"
  `(with-result (answer (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt (format nil ,@prompt)
                          :suggestion-function (yes-no-suggestion-filter)
                          :hide-suggestion-count-p t)))
     (if (confirmed-p answer)
         ,yes-form
         ,no-form)))

(defmacro with-class ((class-sym override-sym) &body body)
  "Dynamically override the class corresponding to CLASS-SYM by OVERRIDE-SYM.
The class is restored when exiting BODY."
  (alex:with-gensyms (old-class)
    `(let ((,old-class (find-class ',class-sym)))
       (unwind-protect
            (progn
              (replace-class ,class-sym ,override-sym)
              ,@body)
         ;; TODO: Test type with CCL:
         (setf (find-class ',class-sym) ,old-class)
         #-SBCL
         (deftype ,class-sym () ',class-sym)))))
