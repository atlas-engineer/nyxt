;; cl-annot does not call `export' at compile time, which fails with ASDF's
;; "compile-bundle-op" (which is needed for Guix).
;; See https://gitlab.common-lisp.net/asdf/asdf/issues/11.

;; cl-annot is no longer maintained, so we hot-patch it here.

(in-package :annot.std)

(defannotation export* (definition-form)
    (:alias export)
  "Export the definition symbol of DEFINITION-FORM."
  (let ((name (definition-form-symbol definition-form)))
    (if name
        `(progn
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (export ',name))
           ,definition-form)
        definition-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :annot.class)

;; WARNING: We need to eval the function overrides at compile time so that they
;; are effectively overridden when `export-accessors' gets expanded.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-accessors-in-defclass* (class-definition-form)
    (loop for slot-specifier in (slot-specifiers class-definition-form)
          for slot-options = (when (consp slot-specifier) (cdr slot-specifier))
          if slot-options
            append (plist-get-all slot-options :reader) into accessors
            and append (plist-get-all slot-options :writer) into accessors
            and append (plist-get-all slot-options :accessor) into accessors
          finally
             (return
               (if accessors
                   `(progn
                      (eval-when (:compile-toplevel :load-toplevel :execute)
                        (export ',accessors))
                      ,class-definition-form)
                   class-definition-form)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-accessors-in-defstruct* (class-definition-form)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',(mapcar (compose
                            (let ((conc-name (get-conc-name class-definition-form)))
                              (if conc-name
                                  (curry #'symbolicate conc-name)
                                  #'identity))
                            #'first
                            #'ensure-list)
                           (slot-specifiers class-definition-form))))
       ,class-definition-form)))

(defmacro export-accessors (class-definition-form)
  (progn-form-replace-last
   (lambda (class-definition-form)
     (case (first class-definition-form)
       (defclass (get-accessors-in-defclass* class-definition-form))
       (defstruct (get-accessors-in-defstruct* class-definition-form))))
   class-definition-form))
