;;; mode.lisp --- Definition of the mode class and root-mode.
;;; All modes inherit from the root-mode.

(in-package :next)

(defmacro define-mode (name direct-superclasses docstring direct-slots &body body)
  "Define mode NAME.
When DIRECT-SUPERCLASSES is T, then the mode has no parents.
Otherwise, the mode's parents are ROOT-MODE and DIRECT-SUPERCLASSES.

BODY is executed during instantiation.
In the BODY, the %MODE variable refers to the mode being instantiated.

A mode toggler is also defined as NAME.
Its arguments are passed to the class instantiation.
To arguments have a special meanings beside the slot value of the mode:
- :BUFFER is used to enable or disable the mode in the corresponding buffer.
- :ACTIVATE is used to choose whether to enable or disable the mode.
If :ACTIVATE is omitted, the mode is toggled."
  `(progn
     (defclass ,name ,(unless (eq (car direct-superclasses) t)
                        (cons 'root-mode direct-superclasses))
       ,direct-slots
       (:documentation ,docstring))
     (defmethod initialize-instance :after ((%mode ,name) &key) ; TODO: Change %mode to ,name?
       ,@body)
     ;; TODO: This should not be a defun, but a define-command, and it should
     ;; call the destructor when toggling off.
     ;; TODO: Can we delete the last mode?  What does it mean to have no mode?
     ;; Should probably always have root-mode.
     (defun ,name (&rest args &key (buffer) (activate t explicit?) &allow-other-keys)
       ,docstring
       (let* ((buffer (or buffer
                          (active-buffer *interface*)))
              (existing-instance (find-if (lambda (m) (eq (class-of m) ',name))
                                          (modes buffer))))
         (unless explicit?
           (setf activate (not existing-instance)))
         (if activate
             (unless existing-instance
               ;; TODO: Should we move mode to the front when it already exists?
               (push (apply #'make-instance ',name
                            :name (format nil "~a" ',name)
                            args)
                     (modes buffer)))
             (when existing-instance
               (when (destructor existing-instance)
                 (funcall (destructor existing-instance)))
               (setf (modes buffer) (delete existing-instance
                                                   (modes buffer)))))))))

(define-mode root-mode (t)
    "The root of all modes."
    ((name :accessor name :initarg :name) ;; TODO: What's the use of mode's NAME slot?
     (buffer :accessor buffer :initarg :buffer)
     (activate :accessor activate :initarg :activate) ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
     (destructor :accessor destructor)  ; TODO: Use it.  Better name?
     (keymap :accessor keymap :initarg :keymap :initform (make-keymap))))

(defun root-mode-default-keymap ()
  "Return the default keymap of root mode."
  (eval (closer-mop:slot-definition-initform
         (find-slot (find-class 'root-mode) 'keymap))))

(defmethod did-commit-navigation ((mode root-mode) url)
  url)

(defmethod did-finish-navigation ((mode root-mode) url)
  url)
