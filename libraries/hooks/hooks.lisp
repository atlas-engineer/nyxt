(in-package :next-hooks)

(defvar *hook* nil
  "The hook currently being run.")

(defgeneric add-hook (hook fn &key append)
  (:documentation "Add FN to the value of HOOK.")
  (:method ((hook symbol) fn &key append)
    (declare (type (or function symbol) fn))
    (serapeum:synchronized (hook)
      (if (not append)
          (pushnew fn (symbol-value hook))
          (unless (member fn (symbol-value hook))
            (alexandria:appendf (symbol-value hook) (list fn)))))))

(defgeneric remove-hook (hook fn)
  (:documentation "Remove FN from the symbol value of HOOK.")
  (:method ((hook symbol) fn)
    (serapeum:synchronized (hook)
      (alexandria:removef (symbol-value hook) fn))))

(defmacro with-hook-restart (&body body)
  `(with-simple-restart (continue "Call next function in hook ~s" *hook*)
     ,@body))

(defun run-hooks (&rest hooks)          ; TODO: What about running hooks over arguments?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass handler ()
  ((name :initarg :name
         :accessor name
         :type symbol
         :initform nil
         :documentation "")
   (description :initarg :description
                :accessor description
                :type string
                :initform ""
                :documentation "")
   (fn :initarg :fn
       :accessor fn
       :type function
       :initform nil
       :documentation "")
   (place :initarg :place
          :accessor place
          :type list
          :initform nil
          :documentation "")
   (value :initarg :value
          :accessor value
          :type t
          :initform nil
          :documentation "")))

(defun make-handler (function &key name place value)
  "NAME is a symbol."
  (setf name (or name (let ((fname (nth-value 2 (function-lambda-expression function))))
                        (when (typep fname 'symbol)
                          fname))))
  (unless name
    (error "Can't make a handler without a name"))
  (make-instance 'handler
                 :name name
                 :fn function
                 :place place
                 :value value))

(defmethod equals ((fn1 handler) (fn2 handler))
  "Return non-nil if FN1 and FN2 are equal."
  (cond
    ((or (and (place fn1)
              (not (place fn2) ))
         (and (place fn2)
              (not (place fn1) )))
     nil)
    ((and (place fn1)
          (place fn2))
     (and (equal (place fn1)
                 (place fn2))
          (equal (value fn1)
                 (value fn2))))
    (t
     (eq (name fn1)
         (name fn2)))))

(defclass hook ()
  ((hook-type :initarg :hook-type
              :accessor hook-type
              :type symbol
              :initform t
              :documentation "
The type of the handlers, typically (function (input types) output-type).")
   (handlers :initarg :handlers
             :accessor handlers
             :type list
             :initform '()
             :documentation "")
   (disabled-handlers :initarg :disabled-handlers
                      :accessor disabled-handlers
                      :type list
                      :initform '()
                      :documentation "Those handlers are not run by `run-hook'.
This is useful it the user wishes to disable some or all handlers without
removing them from the hook.")
   (combination :initarg :combination
                :accessor combination
                :type function
                :initform #'default-combine-hook
                :documentation "
This can be used to reverse the execution order, return a single value, etc.")))

(defmethod default-combine-hook ((hook hook) &rest args)
  "Return the list of the results of the HOOK handlers applied from youngest to oldest to ARGS."
  (mapcar (lambda (handler)
            (with-hook-restart (apply (fn handler) args)))
          (handlers hook)))

(defmethod combine-hook-until-failure ((hook hook) &rest args)
  "Return the list of values until the first nil result.
Handlers after the successful one are not run."
  (let ((result nil))
    (loop for handler in (handlers hook)
          for res = (apply (fn handler) args)
          do (push res result)
          always res)
    (nreverse result)))

(defmethod combine-hook-until-success ((hook hook) &rest args)
  "Return the value of the first non-nil result.
Handlers after the successful one are not run."
  (loop for handler in (handlers hook)
          thereis (apply (fn handler) args)))

(defmethod combine-composed-hook ((hook hook) &rest args)
  "Return the result of the composition of the HOOK handlers on ARGS, from oldest to youngest."
  (apply (apply #'alexandria:compose (mapcar #'fn (handlers hook))) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile-time type checking for add-hook is useful in case `add-hook' is not
;; run when the user config is loaded.  We declaim the type for `add-hook'
;; because `satisfies' takes the object (the `add-hook' function) as argument,
;; but we have no way to access the hook or the handler passed as parameter.

(defmethod add-hook ((hook hook) (handler handler) &key append)
  (serapeum:synchronized (hook)
    (if (not append)
        (pushnew handler (handlers hook) :test #'equals)
        (unless (member handler (handlers hook))
          (alexandria:appendf (symbol-value hook) (list handler))))))

(defmethod add-hook ((hook hook) fn &key append)
  "Add FN to HOOK.
FN cannot be an anonymous function.
To add anonymous functions to HOOK, use

  (add-hook hook (make-handler (lambda () ...) :name ...))"
  (let ((handler (make-handler fn)))
    (add-hook hook handler :append append)))

;; (defmacro add-hook* (hook handler &key append)
;;   (when (hook-type hook)
;;     (assert (typep (fn handler) (hook-type hook)) ((fn handler))
;;             'type-error :datum (fn handler) :expected-type (hook-type hook)))
;;   `(progn
;;      (add-hook ,hook ,handler :append ,append)))

(defmethod remove-hook ((hook hook) (fn handler))
  (serapeum:synchronized (hook)
    (let ((found-handler nil))
      (flet ((find-handler (handlers)
               (setf (handlers hook)
                     (delete-if (lambda (f)
                                  (when (equals f fn)
                                    (setf found-handler f)
                                    t))
                                handlers))))
        (find-handler (handlers hook))
        (unless found-handler
          (find-handler (disabled-handlers hook))))
      found-handler)))

(defmethod remove-hook ((hook hook) handler-name)
  (serapeum:synchronized (hook)
    (let ((found-handler nil))
      (flet ((find-handler (handlers)
               (setf (handlers hook)
                     (delete-if (lambda (f)
                                  (when (eq handler-name (name f))
                                    (setf found-handler f)
                                    t))
                                handlers))))
        (find-handler (handlers hook))
        (unless found-handler
          (find-handler (disabled-handlers hook))))
      found-handler)))

(defmethod run-hook ((hook hook))
  (funcall (combination hook) hook))

;; TODO: Because our hooks are typed, having both `run-hook` and
;; `run-hook-with-args' makes little sense.  Remove `run-hook-with-args'?
(defmethod run-hook-with-args ((hook hook) &rest args)
  (apply (combination hook) hook args))

;; TODO: Implement the following methods? Isn't the `combination' slot more general?
;; (defmethod run-hook-with-args-until-failure ((hook hook) &rest args)
;;   (apply (combination hook) hook args))
;; (defmethod run-hook-with-args-until-success ((hook hook) &rest args)
;;   (apply (combination hook) hook args))

(defmethod disable-hook ((hook hook) &key append)
  "Prepend all active handlers to the list of disabled handlers.
If APPEND is non-nil, append them instead."
  (serapeum:synchronized (hook)
    (setf (disabled-handlers hook)
          (if append
              (append (disabled-handlers hook) (handlers hook))
              (append (handlers hook) (disabled-handlers hook))))
    (setf (handlers hook) nil)))

(defmethod enable-hook ((hook hook) &key append)
  "Prepend all disabled handlers to the list of active handlers.
If APPEND is non-nil, append them instead."
  (serapeum:synchronized (hook)
    (setf (handlers hook)
          (if append
              (append (handlers hook) (disabled-handlers hook))
              (append (disabled-handlers hook) (handlers hook))))
    (setf (disabled-handlers hook) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: cl-hooks uses symbol properties.  Is it a good idea?
;; TODO: Test this!
(defvar %hook-table (make-hash-table :test #'equal)
  "")

(defun define-hook (name &key object type handlers disabled-handlers combination)
  "Create a globally accessible hook."
  (let ((hook
          (make-instance 'hook
                         :type type
                         :handlers handlers
                         :disabled-handlers disabled-handlers
                         :combination combination)))
    (setf (gethash (list name object) %hook-table)
          hook)))

(defun find-hook (name &optional object)
  "Return the global hook with name NAME associated to OBJECT, if provided.
The following examples return different hooks:
- (find-hook 'foo-hook)
- (find-hook 'foo-hook 'bar-class)
- (find-hook 'foo-hook (make-instance 'bar-class))"
  (gethash (list name object) %hook-table))
