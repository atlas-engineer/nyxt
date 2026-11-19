;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *setf-handlers* (sera:dict)
  "A hash-table mapping (CLASS SLOT) pairs to (OBJECT HANDLER) pairs.
OBJECT is an instance of CLASS.
HANDLER is a function that takes a CLASS instance as argument.

See `define-setf-handler'.")

(defvar *setf-handlers-lock* (bt:make-recursive-lock "setf-handlers")
  "Lock serializing access to `*setf-handlers*' and its handler tables.")

(defun invoke-setf-handlers (key instance)
  "Run the handlers registered under KEY on INSTANCE."
  (let ((handlers (bt:with-lock-held (*setf-handlers-lock*)
                    (when-let ((table (gethash key *setf-handlers*)))
                      (alex:hash-table-values table)))))
    (dolist (handler handlers)
      (funcall* handler instance))))

(export-always 'define-setf-handler)
(defmacro define-setf-handler (class-name slot bound-object handler)
  "When (setf (SLOT-WRITER CLASS-INSTANCE) VALUE) is called,
all handlers corresponding to (CLASS SLOT) are evaluated with CLASS-INSTANCE as
argument.

There is a unique HANDLER per BOUND-OBJECT.

When BOUND-OBJECT is garbage-collected, the corresponding handler is
automatically removed."
  (alex:with-gensyms (key installed-p table)
    `(let* ((,key (list (find-class ',class-name) ',slot))
            (,installed-p nil))
       (bt:with-lock-held (*setf-handlers-lock*)
         (let ((,table (gethash ,key *setf-handlers*)))
           (setf ,installed-p (not (null ,table)))
           (unless ,table
             (setf ,table (tg:make-weak-hash-table :test 'equal :weakness :key)
                   (gethash ,key *setf-handlers*) ,table))
           (setf (gethash ,bound-object ,table) ,handler)))
       (unless ,installed-p
         (handler-bind ((warning #'muffle-warning))
           (defmethod (setf ,slot) :after (value (,class-name ,class-name))
             (declare (ignorable value))
             (invoke-setf-handlers ,key ,class-name)))))))

(defun remove-setf-handlers (object)
  "Drop the `define-setf-handler' handlers registered for OBJECT."
  (bt:with-lock-held (*setf-handlers-lock*)
    (maphash (lambda (key handlers)
               (declare (ignore key))
               (remhash object handlers))
             *setf-handlers*)))
