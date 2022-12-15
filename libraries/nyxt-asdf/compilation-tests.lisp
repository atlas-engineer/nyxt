;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt-asdf)

(export-always 'nyxt-compilation-test-system)
(defclass nyxt-compilation-test-system (asdf:system)
  ((packages
    :initform '()  ;; (error "Packages required")
    :initarg :packages
    :reader packages
    :documentation "Packages to check for unbound exports.
Sub-packages are included in the check."))
  (:documentation "Specialized systems for Nyxt compilation tests."))
(import 'nyxt-compilation-test-system  :asdf-user)

(defmethod asdf:component-depends-on ((op asdf:prepare-op) (c nyxt-compilation-test-system))
  `((asdf:load-op "lisp-unit2")
    ,@(call-next-method)))

(defun list-unbound-exports (package)
  (let ((result '()))
    (do-external-symbols (s (find-package package) result)
      (when (and (not (fboundp s))
                 (not (boundp s))
                 (not (find-class s nil))
                 ;; TODO: How can we portably check if symbol refers to a type?
                 #+sbcl
                 (not (sb-ext:defined-type-name-p s))
                 (and (find-package :parenscript)
                      (not (gethash s (symbol-value (find-symbol "*MACRO-TOPLEVEL*" :parenscript))))))
        (push s result )))))

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
      (error "~a~&Found unbound exported symbols in ~a packages."
             report (length report)))))

(defmethod asdf:perform ((op asdf:test-op) (c nyxt-compilation-test-system))
  (mapc #'unbound-exports (packages c)))

(defmethod asdf:perform ((op asdf:load-op) (c nyxt-compilation-test-system))
  (mapc #'unbound-exports (packages c)))
