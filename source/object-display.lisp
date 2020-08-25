;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'object-string)
(defmethod object-string ((object t))
  (princ-to-string object))

(export-always 'object-display)
(defmethod object-display ((object t))
  "Text shown by suggestions in the minibuffer."
  (if (and (mopu:slot-names (class-of object))
           (find (package-name (symbol-package (class-name (class-of object))))
                 (nyxt-packages)
                 :test #'string=))
      (format nil "~a:~%~%~a"
              object
              (format-object object))
      (object-string object)))

(defmethod object-string ((package package))
  (if (eq (package-name package) (find-package :nyxt))
      ""
      (str:replace-all "nyxt/" "" (str:downcase (package-name package)))))

(defun slot-definitions (object)
  (mapcar (lambda (slot-name)
            (list
             slot-name
             (let ((value (slot-value object slot-name)))
               (if (listp value)
                   (trim-list value)
                   value))))
          (mopu:slot-names object)))

(defun format-object (object)
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (pair)
                    (format nil "~a: ~a"
                            (first pair)
                            ;; TODO: We could call object-display and recurse
                            ;; here, but this would require us to implement some
                            ;; form of indentation, lest it becomes unreadable.
                            ;; Better: Make it clickable just like the SLIME inspector.
                            (object-string (second pair))))
                  (slot-definitions object))))
