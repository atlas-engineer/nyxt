;;; hook.lisp --- functions for adding and removing hooks

(in-package :next)

(defun add-function-to-hook (hook-name function-key function)
  (let ((hook-functions-hash (alexandria:ensure-gethash
                              hook-name
                              *available-hooks*
                              (make-hash-table :test #'equalp))))
    (setf (gethash function-key hook-functions-hash) function)))

(defun execute-entry (key value)
  (declare (ignore key))
  (funcall value))

(defun run-hook (hook-name)
  (let ((hook-functions-hash (gethash hook-name *available-hooks*)))
    (when hook-functions-hash
      (maphash 'execute-entry hook-functions-hash))))

(defun remove-hook (hook-name function-key)
  (let ((hook-functions-hash (gethash hook-name *available-hooks*)))
    (remhash function-key hook-functions-hash)))
