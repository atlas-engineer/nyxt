;;; mode.lisp --- Definition of the mode class and root-mode.
;;; All modes inherit from the root-mode.

(in-package :next)

;; TODO: Find a better way to uniquely identidy commands from mode methods.
;; What about symbol properties?  We could use:
;;
;; (setf (get name 'commandp) t)
;;
;; But that doesn't seem to work properly, some commands need to be evaluated
;; twice before they appear in the list.  We could use a class (we used to have
;; a COMMAND class) or intern the symbol into a special package (see `intern'
;; documentation).
(defparameter %%command-list '()
  "The list of known commands, for internal use only.")

;; We need a `command' class for multiple reasons:
;; - Identify commands uniquely (although being a member of `%%command-list' is enough).
;;
;; - Customize minibuffer display value with `object-string'.
;;
;; - Access-time: This is useful to sort command by the time they were last
;;   called.  The only way to do this is to persist the command instances.
(defclass command ()
  ((sym :accessor sym :initarg :sym)
   (mode :accessor mode :initarg :mode) ; TODO: Isn't it better to derive mode dynamically with closer-mop?
   (access-time :accessor access-time :initform 0
                :documentation "Last time this command was called from minibuffer.
This can be used to order the commands.")))

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
     (defclass ,name ,(unless (eq (first direct-superclasses) t)
                        (cons 'root-mode direct-superclasses))
       ,direct-slots
       (:documentation ,docstring))
     (defmethod initialize-instance :after ((%mode ,name) &key) ; TODO: Change %mode to ,name?
       ,@body)
     ;; TODO: This should not be a defun, but a define-command, and it should
     ;; call the destructor when toggling off.
     ;; TODO: Can we delete the last mode?  What does it mean to have no mode?
     ;; Should probably always have root-mode.
     (unless (find-if (lambda (c) (and (eq (sym c) ',name)
                                       (eq (mode c) 'root-mode)))
                      %%command-list)
       (push (make-instance 'command :sym ',name :mode 'root-mode) %%command-list))
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
                         (modes buffer))
                   (echo "~a enabled." ',name))
                 (when existing-instance
                   (when (destructor existing-instance)
                     (funcall (destructor existing-instance) existing-instance))
                   (setf (modes buffer) (delete existing-instance
                                                (modes buffer)))
                   (echo "~a disabled." ',name))))))))

(define-mode root-mode (t)
    "The root of all modes."
    ((name :accessor name :initarg :name) ;; TODO: What's the use of mode's NAME slot?
     (buffer :accessor buffer :initarg :buffer)
     (activate :accessor activate :initarg :activate) ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
     (destructor :accessor destructor  ; TODO: Better name?
                 :documentation
                 "A lambda function which tears down the mode upon deactivation.
It takes the mode as argument.")
     (keymap-schemes :accessor keymap-schemes :initarg :keymap-schemes
                     :initform (list :emacs (make-keymap)))))

(defmethod find-mode ((buffer buffer) mode-symbol)
  "Return the mode corresponding to MODE-SYMBOL in active in BUFFER.
Return nil if mode is not found.
MODE-SYMBOL can be for instance \"'root-mode\"."
  (find-if (lambda (m) (eq mode-symbol (class-name (class-of m))))
           (modes buffer)))

(defun find-buffer (mode-symbol &optional (interface *interface*))
  (find-if (lambda (b)
             (find-mode b mode-symbol))
           (alexandria:hash-table-values (buffers interface))))

(defmethod keymap ((mode root-mode))
  "Return the keymap of MODE according to its buffer keymap scheme.
If there is no corresponding keymap, return nil."
  (getf (keymap-schemes mode)
        (current-keymap-scheme (buffer mode))))

(defmethod did-commit-navigation ((mode root-mode) url)
  url)

(defmethod did-finish-navigation ((mode root-mode) url)
  url)
