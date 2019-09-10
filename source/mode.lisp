;;; mode.lisp --- Definition of the mode class and root-mode.
;;; All modes inherit from the root-mode.

(in-package :next)
(annot:enable-annot-syntax)

@export
(defmacro define-mode (name direct-superclasses docstring direct-slots)
  "Define mode NAME.
When DIRECT-SUPERCLASSES is T, then the mode has no parents.
Otherwise, the mode's parents are ROOT-MODE and DIRECT-SUPERCLASSES.

A mode toggler command is also defined as NAME.
Its arguments are passed to the class instantiation.
Two arguments have a special meaning beside the slot value of the mode:
- :BUFFER is used to enable or disable the mode in the corresponding buffer.
  This should always be specified in Lisp code since the active buffer might, if
  any, might not be the right buffer.
- :ACTIVATE is used to choose whether to enable or disable the mode.
If :ACTIVATE is omitted, the mode is toggled."
  `(progn
     @export
     @export-accessors
     (defclass ,name ,(unless (eq (first direct-superclasses) t)
                        (append direct-superclasses '(root-mode)))
       ,direct-slots
       (:documentation ,docstring))
     ;; TODO: Can we delete the last mode?  What does it mean to have no mode?
     ;; Should probably always have root-mode.
     ,(unless (eq name 'root-mode)
        `(define-command ,name (&rest args &key (buffer (active-buffer *interface*))
                                      (activate t explicit?)
                                      &allow-other-keys)
           ,docstring
           (let ((existing-instance (find-mode buffer ',name)))
               (unless explicit?
                 (setf activate (not existing-instance)))
               (if activate
                   (unless existing-instance
                     ;; TODO: Should we move mode to the front when it already exists?
                     (let ((new-mode (apply #'make-instance ',name
                                            :name (format nil "~a" ',name)
                                            :buffer buffer
                                            args)))
                       (when (constructor new-mode)
                         (funcall (constructor new-mode) new-mode))
                       (push new-mode (modes buffer))
                       (hooks:run-hook (hooks:object-hook new-mode 'enable-hook) new-mode))
                     (echo "~a enabled." ',name))
                   (when existing-instance
                     (hooks:run-hook (hooks:object-hook existing-instance 'disable-hook) existing-instance)
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
   (constructor :accessor constructor :initarg :constructor :type :function :initform nil
                :documentation
                "A lambda function which initializes the mode upon activation.
It takes the mode as argument.")
   (destructor :accessor destructor :initarg :destructor :type :function :initform nil ; TODO: Better name?
               :documentation
               "A lambda function which tears down the mode upon deactivation.
It takes the mode as argument.")
   (keymap-schemes :accessor keymap-schemes :initarg :keymap-schemes :type :list
                   :initform (list :emacs (make-keymap)))
   (enable-hook :accessor enable-hook :initarg :enable-hook :type :list
                :initform '()
                :documentation "This hook is run when enabling the mode.
It takes the mode as argument.")
   (disable-hook :accessor disable-hook :initarg :disable-hook :type :list
                 :initform '()
                 :documentation "This hook is run when disabling the mode.
It takes the mode as argument.")))

@export
(defmethod find-mode ((buffer buffer) mode-symbol)
  "Return the mode corresponding to MODE-SYMBOL in active in BUFFER.
Return nil if mode is not found.  MODE-SYMBOL does not have to be namespaced, it
can be 'document-mode as well as 'next/document-mode:document-mode."
  (let ((mode-full-symbol (if (find-class mode-symbol nil)
                              mode-symbol
                              (match (mode-command mode-symbol)
                                ((guard c (not (null c))) (sym c))))))
    (when mode-full-symbol
      (find-if (lambda (m) (eq mode-full-symbol (class-name (class-of m))))
               (modes buffer)))))

@export
(defun find-buffer (mode-symbol &optional (interface *interface*))
  "Return first buffer matching MODE-SYMBOL."
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
