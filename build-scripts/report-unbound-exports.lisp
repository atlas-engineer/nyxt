;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

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
                      (not (gethash s parenscript::*macro-toplevel*))))
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
      (format t "~a~&Found unbound exported symbols in ~a packages."
              report (length report))
      (uiop:quit 20))))
