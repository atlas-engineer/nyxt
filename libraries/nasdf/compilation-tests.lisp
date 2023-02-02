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
Sub-packages are included in the check."))
  (:documentation "Specialized systems for compilation tests."))
(import 'nasdf-compilation-test-system :asdf-user)

(defun list-unbound-exports (package)
  (let ((result '()))
    (do-external-symbols (s (find-package package) result)
      (when (and (not (fboundp s))
                 (not (boundp s))
                 (not (find-class s nil))
                 ;; TODO: How can we portably check if symbol refers to a type?
                 #+sbcl
                 (not (sb-ext:defined-type-name-p s))
                 (or (not (find-package :parenscript))
                     (not (gethash s (symbol-value (find-symbol "*MACRO-TOPLEVEL*" :parenscript))))))
        (push s result)))))

(defun subpackage-p (subpackage package)
  "Return non-nil if SUBPACKAGE is a sub-package of PACKAGE.
A sub-package has a name that starts with that of PACKAGE followed by a '/' separator."
  (not (null
        (uiop:string-prefix-p (uiop:strcat (package-name package) "/")
                              (package-name subpackage)))))

(defun list-subpackages (package)
  (remove-if (lambda (pkg) (not (subpackage-p pkg package))) (list-all-packages)))

(defun unbound-exports (package)
  "Report unbound exported symbols for PACKAGE and all its subpackages."
  ;; TODO: Only SBCL is supported for now.
  #-sbcl
  nil
  #+sbcl
  (let* ((package (find-package package))
         (report (delete nil
                         (mapcar (lambda (package)
                                   (let ((exports (list-unbound-exports package)))
                                     (when exports
                                       (list package exports))))
                                 (cons (find-package package) (list-subpackages package))))))
    (when report
      (error "~a~&Found unbound exported symbols in ~a package~:p."
             report (length report)))))

(defmethod asdf:perform ((op asdf:test-op) (c nasdf-compilation-test-system))
  (logger "------- STARTING Compilation Testing: ~a" (packages c))
  (mapc #'unbound-exports (packages c))
  (logger "------- ENDING Compilation Testing: ~a" (packages c)))
