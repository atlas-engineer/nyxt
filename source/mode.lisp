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
  This should always be specified in Lisp code since the active buffer might, if
  any, might not be the right buffer.
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
     ,(unless (eq name 'root-mode)
        ;; REVIEW: Here we define the command manually instead of using
        ;; define-command, because this last macro depends on modes and thus
        ;; define-mode itself.
        `(defmethod ,name ((root-mode root-mode) &rest args &key (buffer (active-buffer *interface*)) (activate t explicit?)
                           &allow-other-keys)
           ,docstring
           (let ((existing-instance
                  (find-if (lambda (m)
                             (eq (class-name (class-of m)) ',name))
                           (modes buffer))))
             (unless explicit?
               (setf activate (not existing-instance)))
             (if activate
                 (unless existing-instance
                   ;; TODO: Should we move mode to the front when it already exists?
                   (push (apply #'make-instance ',name
                                :name (format nil "~a" ',name)
                                :buffer buffer
                                args)
                         (modes buffer)))
                 (when existing-instance
                   (when (destructor existing-instance)
                     (funcall (destructor existing-instance) existing-instance))
                   (setf (modes buffer) (delete existing-instance
                                                (modes buffer))))))))))

(define-mode root-mode (t)
    "The root of all modes."
    ((name :accessor name :initarg :name) ;; TODO: What's the use of mode's NAME slot?
     (buffer :accessor buffer :initarg :buffer)
     (activate :accessor activate :initarg :activate) ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
     (destructor :accessor destructor  ; TODO: Better name?
                 :documentation
                 "A lambda function which takes the mode as argument.")
     (keymap-schemes :accessor keymap-schemes :initarg :keymap-schemes
                     :initform (list :emacs (make-keymap)))))

(defmethod keymap ((mode root-mode))
  "Return the keymap of MODE according to its buffer keymap scheme.
If there is no corresponding keymap, return nil."
  (getf (keymap-schemes mode)
        (current-keymap-scheme (buffer mode))))

(defmethod did-commit-navigation ((mode root-mode) url)
  url)

(defmethod did-finish-navigation ((mode root-mode) url)
  url)
