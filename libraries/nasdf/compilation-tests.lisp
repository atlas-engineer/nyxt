;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nasdf)

(export-always 'nasdf-compilation-test-system)
(defclass nasdf-compilation-test-system (nasdf-test-system)
  ((packages
    :initform '()  ;; (error "Packages required")
    :initarg :packages
    :reader packages
    :documentation "Packages to check for unbound exports.
Sub-packages are included in the check.")
   (unbound-symbols-to-ignore
    :initform '()
    :initarg :unbound-symbols-to-ignore
    :reader unbound-symbols-to-ignore
    :documentation "Symbols to ignore when checking for unbound exports.")
   (undocumented-symbols-to-ignore
    :initform '()
    :initarg :undocumented-symbols-to-ignore
    :reader undocumented-symbols-to-ignore
    :documentation "Symbols to ignore when checking for documentation.
Likely, slot names (these don't have native `documentation' support."))
  (:documentation "Specialized systems for compilation tests."))
(import 'nasdf-compilation-test-system :asdf-user)

(defun valid-type-p (type-specifier)
  "Check the TYPE-SPECIFIER for being a valid type.
The logic is:
- If the type is documented as a type, then a type it is.
- Otherwise, if `typep' exits normally (with whatever return value)
  when checking arbitrary value against this specifier, then
  TYPE-SPECIFIER is valid.
- And if there's an error about argument types, then TYPE-SPECIFIER is
  the type requiring arguments. Which means: type exists, even if
  requiring arguments.
- If there's any other error raised by `typep', then TYPE-SPECIFIER is
  likely not a type."
  (or (documentation type-specifier 'type)
      (handler-case
          (progn
            (typep t type-specifier)
            t)
        #+sbcl
        (sb-kernel::arg-count-error ()
          t)
        #+ccl
        (ccl::simple-program-error (e)
          (search "can't be destructured against the lambda list" (format nil "~a" e)))
        #+ecl
        (simple-error (e)
          (or (search "Too few arguments" (format nil "~a" e))
              (not (search "not a valid type specifier" (format nil "~a" e)))))
        #+clisp
        (simple-error (e)
          (or (search "may not be called with 0 arguments" (format nil "~a" e))
              (not (search "invalid type specification" (format nil "~a" e)))))
        (error () nil))))

(defun list-unbound-exports (package)
  (let ((result '()))
    (do-external-symbols (s (find-package package) result)
      (unless (or (fboundp s)
                  (boundp s)
                  (find-class s nil)
                  (valid-type-p s)
                  (and (find-package :parenscript)
                       (gethash s (symbol-value (find-symbol "*MACRO-TOPLEVEL*" :parenscript)))))
        (push s result)))))

(defun subpackage-p (subpackage package)
  "Return non-nil if SUBPACKAGE is a sub-package of PACKAGE.
A sub-package has a name that starts with that of PACKAGE followed by a '/' separator."
  (not (null
        (uiop:string-prefix-p (uiop:strcat (package-name package) "/")
                              (package-name subpackage)))))

(defun list-subpackages (package)
  (remove-if (lambda (pkg) (not (subpackage-p pkg package))) (list-all-packages)))

(defun list-undocumented-exports (package)
  (let ((result '())
        (classes (loop for s being the external-symbol in package
                       when (find-class s nil)
                         collect s)))
    (flet ((accessor-p (symbol)
             "Check whether the SYMBOL is a slot accessor.
This is necessary because accessors rarely have documentation and thus
have to be excluded from the undocumented symbols list.
Uses the built-in MOP abilities of every Lisp."
             (and (fboundp symbol)
                  (typep (symbol-function symbol) 'generic-function)
                  (some (lambda (class)
                          (ignore-errors
                           (typep (find-method (symbol-function symbol) '() (list (find-class class)) nil)
                                  '(or standard-accessor-method standard-reader-method standard-writer-method))))
                        classes))))
      (do-external-symbols (s (find-package package) result)
        (unless (or (some (lambda (doctype) (documentation s doctype))
                          '(variable function compiler-macro setf method-combination type structure))
                    (accessor-p s)
                    ;; Parenscript macros don't have documentation.
                    (and (find-package :parenscript)
                         (gethash s (symbol-value (find-symbol "*MACRO-TOPLEVEL*" :parenscript)))))
          (push s result))))))

(flet ((list-offending-packages (package export-lister testing-for)
         (let* ((package (find-package package)))
           (delete nil
                   (mapcar (lambda (package)
                             (logger ";;; Testing ~a for ~a" package testing-for)
                             (let ((exports (funcall export-lister package)))
                               (when exports
                                 (list package exports))))
                           (cons (find-package package) (list-subpackages package)))))))
  (defun unbound-exports (package symbols-to-ignore)
    "Report unbound exported symbols for PACKAGE and all its subpackages."
    ;; NOTE: these implementations throw errors on atypical type specifier, enabling `valid-type-p'
    #+(or sbcl ccl ecl clisp)
    (let* ((report (list-offending-packages package #'list-unbound-exports "unbound exports"))
           (report (delete
                    nil
                    (mapcar (lambda (rep)
                              (destructuring-bind (package symbols)
                                  rep
                                (let ((really-undocumented-symbols
                                        (remove-if (lambda (sym)
                                                     (member (symbol-name sym) symbols-to-ignore
                                                             :key #'symbol-name :test #'equal))
                                                   symbols)))
                                  (if really-undocumented-symbols
                                      (list package really-undocumented-symbols)
                                      nil))))
                            report))))
      (when report
        (error "~a~&Found unbound exported symbols in ~a package~:p."
               report (length report))))
    #-(or sbcl ccl ecl clisp) nil)

  (defun undocumented-exports (package symbols-to-ignore)
    "Report undocumented exported symbols for PACKAGE and all its subpackages.
SYMBOLS-TO-IGNORE are these that should not be tested for
documentation (e.g. slot names)."
    (let* ((report (list-offending-packages package #'list-undocumented-exports "undocumented exports"))
           (report (delete
                    nil
                    (mapcar (lambda (rep)
                              (destructuring-bind (package symbols)
                                  rep
                                (let ((really-undocumented-symbols
                                        (remove-if (lambda (sym)
                                                     (member (symbol-name sym) symbols-to-ignore
                                                             :key #'symbol-name :test #'equal))
                                                   symbols)))
                                  (if really-undocumented-symbols
                                      (list package really-undocumented-symbols)
                                      nil))))
                            report))))
      (when report
        (error "~a~&Found undocumented exported symbols in ~a package~:p."
               report (length report))))))

(defmethod asdf:perform ((op asdf:test-op) (c nasdf-compilation-test-system))
  (logger "------- STARTING Compilation Testing: ~a" (packages c))
  (mapc #'(lambda (p) (unbound-exports p (unbound-symbols-to-ignore c))) (packages c))
  (mapc #'(lambda (p) (undocumented-exports p (undocumented-symbols-to-ignore c))) (packages c))
  (logger "------- ENDING Compilation Testing: ~a" (packages c)))
