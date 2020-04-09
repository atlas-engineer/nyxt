;;; autofill.lisp --- functions to enable auto filling of input fields

(in-package :next)

(defstruct autofill
  (id)
  (key)
  (fill))

(defmethod object-string ((autofill autofill))
  (autofill-key autofill))

(defmethod object-display ((autofill autofill))
  (format nil "~a:  ~a" (autofill-key autofill)
          (cond ((stringp (autofill-fill autofill))
           (autofill-fill autofill))
          ((functionp (autofill-fill autofill))
           "Function"))))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (with-result (selected-fill (read-from-minibuffer
                               (make-minibuffer
                                :input-prompt "Autofill"
                                :completion-function
                                (lambda (input)
                                  (fuzzy-match input (autofills *browser*))))))
    (cond ((stringp (autofill-fill selected-fill))
           (%paste :input-text (autofill-fill selected-fill)))
          ((functionp (autofill-fill selected-fill))
           (%paste :input-text (funcall (autofill-fill selected-fill)))))))
