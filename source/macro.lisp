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
