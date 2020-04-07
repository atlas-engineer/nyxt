;;; autofill.lisp --- functions to enable auto filling of input fields

(in-package :next)

(defstruct autofill
  (id)
  (key)
  (fill))

(defmethod object-string ((autofill autofill))
  (autofill-key autofill))

(defmethod object-display ((autofill autofill))
  (format nil "~a  ~a" (autofill-key autofill) (autofill-fill autofill)))

(define-command autofill ()
  "Fill in a field with a value from a saved list."
  (with-result (selected-fill (read-from-minibuffer
                        (make-minibuffer
                         :input-prompt "Autofill"
                         :completion-function
                         (lambda (input)
                           (fuzzy-match input (autofills *browser*))))))
    (%paste :input-text (autofill-fill selected-fill))))
