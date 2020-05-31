(in-package :next)

(export-always '(autofill-id
                 autofill-name
                 autofill-key
                 autofill-fill))
(defstruct autofill
  (id)
  (name)
  (key)
  (fill))

(defmethod object-string ((autofill autofill))
  (autofill-key autofill))

(defmethod object-display ((autofill autofill))
  (format nil "~a:  ~a" (autofill-key autofill)
          (cond ((stringp (autofill-fill autofill))
           (autofill-fill autofill))
          ((functionp (autofill-fill autofill))
           (or (autofill-name autofill) "Function")))))
