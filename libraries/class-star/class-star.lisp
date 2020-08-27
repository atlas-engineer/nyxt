(in-package :class*)

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

(defmacro with-class ((class-sym override-sym) &body body)
  "Dynamically override the class corresponding to CLASS-SYM by OVERRIDE-SYM.
The class is restored when exiting BODY."
  (alexandria:with-gensyms (old-class)
    `(let ((,old-class (find-class ',class-sym)))
       (unwind-protect
            (progn
              (replace-class ,class-sym ,override-sym)
              ,@body)
         ;; TODO: Test type with CCL:
         (setf (find-class ',class-sym) ,old-class)
         #-SBCL
         (deftype ,class-sym () ',class-sym)))))

(defun original-class (class-sym)
  "Return the parent class with the same name, or nil if there is none.
This is useful to retrieve the original class of a class that was overridden,
e.g. with (define-class foo (foo) ...)."
  (find class-sym (mopu:superclasses (find-class class-sym))
        :key #'class-name))

(defun name-identity (name definition)
  (declare (ignore definition))
  name)

(defun superclasses-have-cycle? (name supers)
  (and (find-class name nil)
           ;; mopu:superclasses can be expansive, avoid calling it if first
           ;; condition is enough.
           (or (member (find-class name) (mapcar #'find-class supers))
               (member (find-class name) (mapcar #'mopu:superclasses supers)))))

(defun initform (definition)
  "Return (BOOLEAN INITFORM) when initform is found."
  (let ((definition (rest definition)))
    (if (oddp (length definition))
        (values t (first definition))
        (multiple-value-bind (found? value)
            (get-properties definition '(:initform))
          (values (not (null found?)) value)))))

(defun definition-type (definition)
  "Return definition's TYPE.
Return nil if not found."
  (let ((definition (rest definition)))
    (when (oddp (length definition))
      (setf definition (rest definition)))
    (getf definition :type)))

(defun basic-type-zero-values (type)
  "Return TYPE zero value.
An error is raised if the type is unsupported."
  (cond
    ((subtypep type 'string) "")
    ((subtypep type 'boolean) nil)
    ((subtypep type 'list) '())
    ((subtypep type 'array) (make-array 0))
    ((subtypep type 'hash-table) (make-hash-table))
    ;; Order matters for numbers:
    ((subtypep type 'integer) 0)
    ((subtypep type 'complex) #c(0 0))
    ((subtypep type 'number) 0.0)
    (t (error "Unknown type"))))

(defun type-zero-initform-inference (definition)
  "Infer basic type zero values.
See `basic-type-zero-values'.
Raise a condition at macro-expansion time when initform is missing for unsupported types."
  (let ((type (definition-type definition)))
    (if type
        (handler-case (basic-type-zero-values type)
          (error ()
            ;; Compile-time error:
            (error "Missing initform.")))
        ;; Default initform when type is missing:
        nil)))

(defun no-unbound-initform-inference (definition)
  "Infer basic type zero values.
Raise a condition when instantiating if initform is missing for unsupported types."
  (let ((type (definition-type definition)))
    (if type
        (handler-case (basic-type-zero-values type)
          (error ()
            ;; Run-time error:
            '(error "Slot must be bound.")))
        ;; Default initform when type is missing:
        nil)))

(defun nil-fallback-initform-inference (definition)
  "Infer basic type zero values.
Fall back to nil if initform is missing for unsupported types."
  (let ((type (definition-type definition)))
    (if type
        (handler-case (basic-type-zero-values type)
          (error ()
            ;; Fall-back to nil:
            nil))
        ;; Default initform when type is missing:
        nil)))

(defvar *initform-inference* 'type-zero-initform-inference)

(defun process-slot-initform (definition &optional initform-inference) ; See `hu.dwim.defclass-star:process-slot-definition'.
  (unless (consp definition)
    (setf definition (list definition)))
  (if (initform definition)
      definition
      (let ((type (getf (rest definition) :type)))
        (setf definition (append definition
                                 (list :initform (if (and type initform-inference)
                                                     (funcall initform-inference definition)
                                                     nil)))))))

(defmacro define-class (name supers &body (slots . options))
  "Define class like `defclass*' but with extensions.

The default initforms can be automatically inferred by the f unction specified
in the :initform-inference option, which defaults to `*initform-inference*'.
The initform can still be specified manually with `:initform' or as second
argument, right after the slot name.

This class definition macro supports cycle in the superclasses,
e.g.  (define-class foo (foo) ()) works."
  (if (superclasses-have-cycle? name supers)
      (let ((temp-name (gensym (string name))))
        ;; TODO: Don't export the class again.
        `(progn (hu.dwim.defclass-star:defclass* ,temp-name ,supers
                  ,(mapcar #'process-slot-initform slots)
                  ,@options)
                (setf (find-class ',name) (find-class ',temp-name))))
      (let* ((option-type (assoc :initform-inference options))
             (initform-inference (or (when option-type
                                       (setf options (delete :initform-inference options :key #'car))
                                       (eval (second option-type)))
                                     *initform-inference*)))
        `(hu.dwim.defclass-star:defclass* ,name ,supers
           ,(mapcar (lambda (definition)
                      (process-slot-initform definition initform-inference))
                    slots)
           ,@options))))
