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
         :documentation "
Name of the handler.
If defaults to the function name if `fn' is a named function.
This is useful so that the user can build handlers out of anonymous functions.")
   (description :initarg :description
                :accessor description
                :type string
                :initform ""
                :documentation "
Description of the handler.  This is purely informative.")
   (fn :initarg :fn
       :accessor fn
       :type function
       :initform nil
       :documentation "
The handler function.  It can be an anonymous function.")
   (handler-type :initarg :handler-type
                 :accessor handler-type
                 :type list
                 :initform nil
                 :documentation "The function type of FN.
This is purely informative.")
   (place :initarg :place
          :accessor place
          :type (or symbol list)
          :initform nil
          :documentation "
If the handler is meant to be a setter, PLACE describes what is set.
PLACE can be a symbol or a pair (CLASS SLOT).
This can be left empty if the handler is not a setter.")
   (value :initarg :value
          :accessor value
          :type t
          :initform nil
          :documentation "
If the handler is meant to be a setter, VALUE can be used to describe what FN is
going to set to PLACE.
In particular, PLACE and VALUE can be used to compare handlers.
This can be left empty if the handler is not a setter.")))

;; TODO: Maybe don't export make-handler, so that user is forced to use the
;; typed versions.
(defun make-handler (fn &key (class-name 'handler) name handler-type place value)
  "NAME is a symbol."
  (setf name (or name (let ((fname (nth-value 2 (function-lambda-expression fn))))
                        (when (typep fname 'symbol)
                          fname))))
  (unless name
    (error "Can't make a handler without a name"))
  (make-instance class-name
                 :name name
                 :fn fn
                 :handler-type handler-type
                 :place place
                 :value value))

(defmethod equals ((fn1 handler) (fn2 handler))
  "Return non-nil if FN1 and FN2 are equal.
Handlers are equal if they are setters of the same place and same value, or if
their names are equal."
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
  ((handler-class :reader handler-class
                  :type symbol
                  :initform t
                  :documentation "
The class of the supported handlers.")
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
  "Return the list of the results of the HOOK handlers applied from youngest to
oldest to ARGS.
Return '() when there is no handler.
This is an acceptable `combination' for `hook'."
  (mapcar (lambda (handler)
            (with-hook-restart (apply (fn handler) args)))
          (handlers hook)))

(defmethod combine-hook-until-failure ((hook hook) &rest args) ; TODO: Result difference between "all fail" and "no handlers"?
  "Return the list of values until the first nil result.
Handlers after the successful one are not run.
This is an acceptable `combination' for `hook'."
  (let ((result nil))
    (loop for handler in (handlers hook)
          for res = (apply (fn handler) args)
          do (push res result)
          always res)
    (nreverse result)))

(defmethod combine-hook-until-success ((hook hook) &rest args) ; TODO: Result difference between "no success" and "no handlers"?
  "Return the value of the first non-nil result.
Handlers after the successful one are not run.
This is an acceptable `combination' for `hook'."
  (loop for handler in (handlers hook)
     thereis (apply (fn handler) args)))

(defmethod combine-composed-hook ((hook hook) &rest args)
  "Return the result of the composition of the HOOK handlers on ARGS, from
oldest to youngest.
Without handler, return ARGS as values.
This is an acceptable `combination' for `hook'."
  (let ((fn-list (mapcar #'fn (handlers hook))))
    (if fn-list
        (apply (apply #'alexandria:compose fn-list) args)
        (values-list args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-hook-internal (hook handler &key append)
  (serapeum:synchronized (hook)
    (if (not append)
        (pushnew handler (handlers hook) :test #'equals)
        (unless (member handler (handlers hook))
          (alexandria:appendf (symbol-value hook) (list handler))))))

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

;; TODO: Add `disable-handler' and `enable-handler'?  Or simply add `handlers' argument to disable-hook / enable-hook?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks.

;; TODO: cl-hooks uses symbol properties.  Is it a good idea?
(defvar %hook-table (make-hash-table :test #'equal)
  "Global hook table.")

(defun define-hook (hook-type name &key object handlers disabled-handlers combination)
  "Return a globally-accessible hook.
The hook can be accessed with `find-hook' at (list NAME OBJECT)."
  (let ((hook
          (make-instance hook-type
                         :handlers handlers
                         :disabled-handlers disabled-handlers
                         :combination combination)))
    (setf (gethash (list name object) %hook-table)
          hook)
    hook))

(defun find-hook (name &optional object)
  "Return the global hook with name NAME associated to OBJECT, if provided.
The following examples return different hooks:
- (find-hook 'foo-hook)
- (find-hook 'foo-hook 'bar-class)
- (find-hook 'foo-hook (make-instance 'bar-class))"
  (gethash (list name object) %hook-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile-time type checking for add-hook is useful in case `add-hook' is not
;; run when the user config is loaded.  Sadly Common Lisp does not seem to allow
;; to extract the type of a function so that it can be checked against what a
;; hook would allow.  We work around this issue with a macro:
;;
;; - Generates hook and handler subclasses.
;; - Generate a handler maker and declaim provided function type.
;; - Generate a `add-hook' defmethod against those 2 classes.

(defmacro define-hook-type (name type)
  "Define hook class and constructor and the associated handler class.
Type must be something like:

  (function (string) (values integer t))

A function with name make-handler-NAME will be created.
A class with name handler-NAME will be created.
The method `add-hook' is added for the new hook and handler types."
  (let* ((name (string name))
         (function-name (intern (str:concat "MAKE-HANDLER-" name)))
         (handler-class-name (intern (str:concat "HANDLER-" name)))
         (hook-class-name (intern (str:concat "HOOK-" name))))
    `(progn
       (defclass ,handler-class-name (handler) ())
       (declaim (ftype (function (,type &key (:name symbol) (:place t) (:value t)))
                       ,function-name))
       (defun ,function-name (fn &key name place value)
         (make-handler fn :class-name ',handler-class-name
                          :name name
                          :handler-type ',type
                          :place place
                          :value value))
       (defclass ,hook-class-name (hook)
         ((handler-class :initform (find-class ',handler-class-name))))
       (defmethod add-hook ((hook ,hook-class-name) (handler ,handler-class-name) &key append)
         ,(format nil "Add HANDLER to HOOK.
HOOK must be of type ~a
HANDLER must be of type ~a."
                  hook-class-name
                  handler-class-name)
         (add-hook-internal hook handler :append append)))))

;; TODO: Add make-hook-* function?
;; TODO: Allow listing all the hooks?

(define-hook-type void (function ()))
(define-hook-type string->string (function (string) string))
(define-hook-type number->number (function (number) number))
(define-hook-type any (function (&rest t)))
