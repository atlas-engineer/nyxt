(in-package :nyxt)

(export-always 'define-mode)
(defmacro define-mode (name direct-superclasses &body body)
  "Define mode NAME.
When DIRECT-SUPERCLASSES is T, then the mode has no parents.
Otherwise, the mode's parents are ROOT-MODE and DIRECT-SUPERCLASSES.

As a third argument, a documentation-string can be provided.
The last argument (third if no doc-string provided, fourth if one is)
is the direct-slots list.

A mode toggler command is also defined as NAME.
Its arguments are passed to the class instantiation.
Two key arguments have a special meaning beside the slot value of the mode:
- :BUFFER is used to enable or disable the mode in the corresponding buffer.
  This should always be specified in Lisp code since the active buffer might, if
  any, might not be the right buffer.
- :ACTIVATE is used to choose whether to enable or disable the mode.
If :ACTIVATE is omitted, the mode is toggled.
The buffer is returned so that mode toggles can be chained.

Example:

\(define-mode my-mode ()
  \"Dummy mode for the custom key bindings in `*my-keymap*'.\"
  ((keymap-schemes :initform (keymap:make-scheme
                              scheme:emacs *my-keymap*
                              scheme:vi-normal *my-keymap*))))"
  (let* ((class-var (intern (format nil "*~a-CLASS*" name)))
         (docstring (if (stringp (first body))
                        (first body)
                        (progn
                          (log:warn "The define-mode definition for ~a doesn't have a documentation string." name)
                          nil)))
         (direct-slots (if (stringp (first body))
                           (cadr body)
                           (first body)))
         (class-args `(,name
                       ,(unless (eq (first direct-superclasses) t)
                          (append direct-superclasses '(root-mode)))
                       ,direct-slots)))
    (when docstring
      (setf class-args (append class-args
                               `((:documentation ,docstring)))))
    `(progn
       (defclass-export ,@class-args)
       ;; Class symbol customization:
       (define-class-type ,name)
       (declaim (type (,(intern (format nil "~a-TYPE" name))) ,class-var))
       (export-always ',class-var)
       (defparameter ,class-var ',name
         ,(format nil "Default class to use for ~a." name))
       ;; TODO: Can we delete the last mode?  What does it mean to have no mode?
       ;; Should probably always have root-mode.
       ,(unless (eq name 'root-mode)
          `(define-command ,name (&rest args &key (buffer (current-buffer))
                                        (activate t explicit?)
                                        &allow-other-keys)
             ,docstring
             (unless (typep buffer 'buffer)
               (error ,(format nil "Mode command ~a called on empty buffer" name)))
             (let ((existing-instance (find-mode buffer ,class-var)))
               (unless explicit?
                 (setf activate (not existing-instance)))
               (if activate
                   (unless existing-instance
                     ;; TODO: Should we move mode to the front when it already exists?
                     (let ((new-mode (apply #'make-instance ,class-var
                                            :buffer buffer
                                            args)))
                       (when (constructor new-mode)
                         (funcall-safely (constructor new-mode) new-mode))
                       (push new-mode (modes buffer))
                       (hooks:run-hook (enable-hook new-mode) new-mode)
                       (hooks:run-hook (enable-mode-hook buffer) new-mode))
                     (print-status)
                     (log:debug "~a enabled." ',name))
                   (when existing-instance
                     (hooks:run-hook (disable-hook existing-instance) existing-instance)
                     (hooks:run-hook (disable-mode-hook buffer) existing-instance)
                     (when (destructor existing-instance)
                       (funcall-safely (destructor existing-instance) existing-instance))
                     (setf (modes buffer) (delete existing-instance
                                                  (modes buffer)))
                     (print-status)
                     (log:debug "~a disabled." ',name))))
             buffer)))))


(defclass root-mode () ())
(hooks:define-hook-type mode (function (root-mode)))

(define-mode root-mode (t)
  "All modes inherit from `root-mode'."
  ((buffer :accessor buffer
           :initarg :buffer
           :initform nil
           :type (or buffer null))
   (activate :accessor activate :initarg :activate) ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
   (constructor :accessor constructor
                :initarg :constructor
                :type (or function null)
                :initform nil ; TODO: Make constructor / destructor methods?  Then we can use initialize-instance, etc.
                :documentation
                "A lambda function which initializes the mode upon activation.
It takes the mode as argument.")
   (destructor :accessor destructor
               :initarg :destructor
               :type (or function null)
               :initform nil ; TODO: Better name?
               :documentation
               "A lambda function which tears down the mode upon deactivation.
It takes the mode as argument.")
   (enable-hook :accessor enable-hook :initarg :enable-hook
                :initform (make-hook-mode)
                :type hook-mode
                :documentation "This hook is run when enabling the mode.
It takes the mode as argument
It is run before the destructor.")
   (disable-hook :accessor disable-hook :initarg :disable-hook
                 :initform (make-hook-mode)
                 :type hook-mode
                 :documentation "This hook is run when disabling the mode.
It takes the mode as argument.
It is run before the destructor.")
   (keymap-scheme :accessor keymap-scheme
                  :initarg :keymap-scheme
                  :type keymap:scheme
                  :initform (make-hash-table :size 0))))

(defmethod object-string ((mode root-mode))
  (symbol-name (class-name (class-of mode))))

(export-always 'find-mode)
(defmethod find-mode ((buffer buffer) mode-symbol)
  "Return the mode corresponding to MODE-SYMBOL in active in BUFFER.
Return nil if mode is not found.  MODE-SYMBOL does not have to be namespaced, it
can be 'web-mode as well as 'nyxt/web-mode:web-mode."
  (let ((mode-full-symbol (if (find-class mode-symbol nil)
                              mode-symbol
                              (match (mode-command mode-symbol)
                                ((guard c (not (null c))) (sym c))))))
    (when mode-full-symbol
      (find-if (lambda (m) (eq mode-full-symbol (class-name (class-of m))))
               (modes buffer)))))

(export-always 'find-submode)
(defmethod find-submode ((buffer buffer) mode-symbol)
  "Like `find-mode' but return the first mode in BUFFER that is a sub-mode of MODE-SYMBOL.
It may be MODE-SYMBOL itself."
  (let ((mode-full-symbol (if (find-class mode-symbol nil)
                              mode-symbol
                              (match (mode-command mode-symbol)
                                ((guard c (not (null c))) (sym c))))))
    (when mode-full-symbol
      (find-if (lambda (m)
                 (closer-mop:subclassp (class-of m)
                                       (find-class mode-full-symbol)))
               (modes buffer)))))

(export-always 'find-buffer)
(defun find-buffer (mode-symbol)
  "Return first buffer matching MODE-SYMBOL."
  (find-if (lambda (b)
             (find-mode b mode-symbol))
           (buffer-list)))

(export-always 'keymap)
(defmethod keymap ((mode root-mode))
  "Return the keymap of MODE according to its buffer keymap scheme.
If there is no corresponding keymap, return nil."
  (keymap:get-keymap (if (buffer mode)
                         (keymap-scheme-name (buffer mode))
                         scheme:cua)
                     (keymap-scheme mode)))

(defmethod on-signal-notify-uri ((mode root-mode) url)
  (set-window-title)
  (print-status)
  url)

(defmethod on-signal-notify-title ((mode root-mode) title)
  (on-signal-notify-uri mode (url (buffer mode)))
  title)

(defmethod on-signal-load-committed ((mode root-mode) url)
  url)

(defmethod on-signal-load-finished ((mode root-mode) url)
  url)

(defmethod s-serialization:serializable-slots ((object root-mode))
  "Discard keymaps which can be quite verbose."
  (delete 'keymap-scheme
          (mapcar #'closer-mop:slot-definition-name
                  (closer-mop:class-slots (class-of object)))))
