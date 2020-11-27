;;;; SPDX-License-Identifier: Public Domain

;; This is a hotpatch for hu.dwim.defclass-star which is under the public
;; domain.

(in-package hu.dwim.defclass-star)

(defun process-slot-definition (definition)
  (unless (consp definition)
    (setf definition (list definition)))
  (let ((name (pop definition))
        (initform 'missing)
        (entire-definition definition))
    (assert name)
    (push name *slot-names*)
    (if (oddp (length definition))
        (progn
          (setf initform (pop definition))
          (setf entire-definition definition)
          (when (eq initform :unbound)
            (setf initform 'missing)))
        (setf initform (getf definition :initform 'missing)))
    (assert (every #'keywordp (loop for el :in definition :by #'cddr
                                    collect el))
            () "Found non-keywords in ~S" definition)
    (destructuring-bind (&key (accessor 'missing) (initarg 'missing)
                              (reader 'missing) (writer 'missing)
                              (export 'missing)
                              &allow-other-keys)
        definition
      (remf-keywords definition :accessor :reader :writer :initform :initarg :export)
      (let ((unknown-keywords (loop for el :in definition :by #'cddr
                                    unless (or (member t *allowed-slot-definition-properties*)
                                               (member el *allowed-slot-definition-properties*))
                                    collect el))
            (slot-name-warning-triggered? nil))
        (when unknown-keywords
          (style-warn "Unexpected properties in slot definition ~S.~%~
                       The unexpected properties are ~S.~%~
                       To avoid this warning (pushnew (or T :your-custom-keyword) hu.dwim.defclass-star:*allowed-slot-definition-properties*)"
                      entire-definition unknown-keywords))
        (flet ((provided-p (value)
                 (and value
                      (not (eq value 'missing))))
               (transform-accessor ()
                 (funcall *accessor-name-transformer* name entire-definition))
               (maybe-warn-for-slot-name ()
                 (unless (or slot-name-warning-triggered?
                             (eq (symbol-package name) *package*))
                   (setf slot-name-warning-triggered? t)
                   #+nil ;; this generates too many warnings which makes it kinda pointless
                   (style-warn "defclass* for a slot name ~A while its home package is not *package* (~A). Default generated names will be interned into *package*!"
                               (fully-qualified-symbol-name name) *package*))))
          (prog1
              (funcall *slot-definition-transformer*
                       (append (list name)
                               (unless (eq initform 'missing)
                                 (list :initform initform))
                               (if (and (eq accessor 'missing)
                                        (eq reader 'missing)
                                        (eq writer 'missing))
                                   (when *automatic-accessors-p*
                                     (maybe-warn-for-slot-name)
                                     (setf accessor (transform-accessor))
                                     (list :accessor accessor))
                                   (let ((transformed-accessor (transform-accessor)))
                                     (append (progn
                                               (when (eq accessor t)
                                                 (setf accessor transformed-accessor))
                                               (when (provided-p accessor)
                                                 (list :accessor accessor)))
                                             (progn
                                               (when (eq reader t)
                                                 (setf reader transformed-accessor))
                                               (when (provided-p reader)
                                                 (list :reader reader)))
                                             (progn
                                               (when (eq writer t)
                                                 (setf writer `(setf ,transformed-accessor)))
                                               (when (provided-p writer)
                                                 (list :writer writer))))))
                               (if (eq initarg 'missing)
                                   (when *automatic-initargs-p*
                                     (list :initarg (funcall *initarg-name-transformer* name entire-definition)))
                                   (when initarg
                                     (list :initarg initarg)))
                               definition))
            (when (provided-p accessor)
              (pushnew accessor *accessor-names*))
            (when (provided-p reader)
              (pushnew reader *accessor-names*))
            (when (provided-p writer)
              (pushnew (second writer) *accessor-names*))
            (if (not (eq export 'missing))
                (ecase export
                  (:accessor
                   (when accessor
                     (push accessor *symbols-to-export*)))
                  ((:slot :name :slot-name)
                   (push name *symbols-to-export*))
                  ((t)
                    ;; XXX: The `when' condition was just `accessor' and failed to account for missing accessors.
                   (when (and accessor (not (eq accessor 'missing)))
                     (push accessor *symbols-to-export*))
                   (push name *symbols-to-export*))
                  ((nil)))
                (progn
                  (when *export-accessor-names-p*
                    ;; XXX: Same as above.
                    (when (and accessor (not (eq accessor 'missing)))
                      (push accessor *symbols-to-export*)))
                  (when *export-slot-names-p*
                    (push name *symbols-to-export*))))))))))
