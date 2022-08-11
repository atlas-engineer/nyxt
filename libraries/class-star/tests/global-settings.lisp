;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :class-star/tests)

;; We use a dedicated file to test compile-time settings, so that they don't spill over.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf hu.dwim.defclass-star:*automatic-initargs-p* nil))

(class-star:define-class foo-no-initarg ()
  ((name :type string)))

(define-test no-initarg ()
  (assert-error #+sbcl
                'sb-pcl::initarg-error
                #-sbcl
                'error
                (make-instance 'foo-no-initarg :name "bar")))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf hu.dwim.defclass-star:*automatic-initargs-p* t))
