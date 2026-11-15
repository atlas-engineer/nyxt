;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defvar *setf-handlers* (sera:dict)
  "A hash-table mapping (CLASS SLOT) pairs to (OBJECT HANDLER) pairs.
OBJECT is an instance of CLASS.
HANDLER is a function that takes a CLASS instance as argument.

See `define-setf-handler'.")

(export-always 'define-setf-handler)
(defmacro define-setf-handler (class-name slot bound-object handler)
  "When (setf (SLOT-WRITER CLASS-INSTANCE) VALUE) is called,
all handlers corresponding to (CLASS SLOT) are evaluated with CLASS-INSTANCE as
argument.

There is a unique HANDLER per BOUND-OBJECT.

When BOUND-OBJECT is garbage-collected, the corresponding handler is
automatically removed."
  (alex:with-gensyms (key)
    `(let ((,key (list (find-class ',class-name) ',slot)))
       (handler-bind ((warning (if (gethash ,key *setf-handlers*)
                                   #'muffle-warning
                                   #'identity)))
         (setf (gethash ,bound-object
                        (alex:ensure-gethash
                         ,key
                         *setf-handlers*
                         (tg:make-weak-hash-table :test 'equal
                                                  :weakness :key)))
               ,handler)
         (defmethod (setf ,slot) :after (value (,class-name ,class-name))
           (declare (ignorable value))
           (dolist (handler (alex:hash-table-values
                             (gethash ,key *setf-handlers*)))
             (funcall* handler ,class-name)))))))

(defun remove-setf-handlers (object)
  "Drop the `define-setf-handler' handlers registered for OBJECT."
  (maphash (lambda (key handlers)
             (declare (ignore key))
             (remhash object handlers))
           *setf-handlers*))
