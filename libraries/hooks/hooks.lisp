(defvar *hook* nil
  "The hook currently being run.")

(defgeneric add-hook (hook fn &key append)
  (:documentation "Add FN to the value of HOOK.")
  (:method ((hook symbol) fn &key append)
    (declare (type (or function symbol) fn))
    (synchronized (hook)
      (if (not append)
          (pushnew fn (symbol-value hook))
          (unless (member fn (symbol-value hook))
            (appendf (symbol-value hook) (list fn)))))))

(defgeneric remove-hook (hook fn)
  (:documentation "Remove FN from the symbol value of HOOK.")
  (:method ((hook symbol) fn)
    (synchronized (hook)
      (removef (symbol-value hook) fn))))

(defmacro with-hook-restart (&body body)
  `(with-simple-restart (continue "Call next function in hook ~s" *hook*)
     ,@body))

(defun run-hooks (&rest hooks)
  "Run all the hooks in HOOKS.
The variable `*hook*' is bound to the name of each hook as it is being
run."
  (dolist (*hook* hooks)
    (run-hook *hook*)))

(defgeneric run-hook (hook)
  (:documentation "Run the functions in HOOK.")
  (:method ((*hook* symbol))
    (dolist (fn (symbol-value *hook*))
      (with-hook-restart
        (funcall fn)))))

(defgeneric run-hook-with-args (hook &rest args)
  (:documentation "Apply each function in HOOK to ARGS.")
  (:method ((*hook* symbol) &rest args)
    (dolist (fn (symbol-value *hook*))
      (with-hook-restart
        (apply fn args)))))

(defgeneric run-hook-with-args-until-failure (hook &rest args)
  (:documentation "Like `run-hook-with-args', but quit once a function returns nil.")
  (:method ((*hook* symbol) &rest args)
    (loop for fn in (symbol-value *hook*)
          always (apply fn args))))

(defgeneric run-hook-with-args-until-success (hook &rest args)
  (:documentation "Like `run-hook-with-args', but quit once a function returns
non-nil.")
  (:method ((*hook* symbol) &rest args)
    (loop for fn in (symbol-value *hook*)
            thereis (apply fn args))))
