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

(defun type-zero-value (type) ; TODO: Make this function customizable.
  (if type
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
                                        ; TODO: Make fallback customizable, 3 options: unbound, (error), compile-time (error)
        ;; Maybe in the caller instead?  Both for finest control?
        (t (error "Missing initform.")))
      ;; TODO: What value do we return here?
      nil))

(defun process-slot-initform (definition) ; See `hu.dwim.defclass-star:process-slot-definition'.
  (unless (consp definition)
    (setf definition (list definition)))
  (if (initform definition)             ; TODO: Add global option to decide what to do when initform _and/or_ type are missing.
      definition
      (let ((type (getf (rest definition) :type)))
        (setf definition (append definition
                                 (list :initform (if type
                                                     (type-zero-value type)
                                                     nil)))))))

(defmacro define-class (name supers &body (slots . options))
  "Define class like `defclass*' but with extensions.

The default initforms is automatically inferred to the zero value of the type,
or nil if there is no type.
The initform can still be specified manually with `:initform'.

This class definition macro supports cycle in the superclasses,
e.g.  (define-class foo (foo) ()) works."
  (if (superclasses-have-cycle? name supers)
      (let ((temp-name (gensym (string name))))
        ;; TODO: Don't export the class again.
        `(progn (hu.dwim.defclass-star:defclass* ,temp-name ,supers
                  ,(mapcar #'process-slot-initform slots)
                  ,@options)
                (setf (find-class ',name) (find-class ',temp-name))))
      `(hu.dwim.defclass-star:defclass* ,name ,supers
         ,(mapcar #'process-slot-initform slots)
         ,@options)))
