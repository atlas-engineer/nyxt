;;; mode.lisp --- Definition of the mode class and root-mode.
;;; All modes inherit from the root-mode.

(in-package :next)

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
                       (hooks:run-hook (enable-hook new-mode) new-mode))
                     (print-status)
                     (log:debug "~a enabled." ',name))
                   (when existing-instance
                     (hooks:run-hook (disable-hook existing-instance) existing-instance)
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
  "The root of all modes."
  ((buffer :accessor buffer :initarg :buffer)
   (activate :accessor activate :initarg :activate) ; TODO: This can be used in the future to temporarily turn off modes without destroying the object.
   (constructor :accessor constructor :initarg :constructor :type :function :initform nil ; TODO: Make constructor / destructor methods?  Then we can use initialize-instance, etc.
                :documentation
                "A lambda function which initializes the mode upon activation.
It takes the mode as argument.")
   (destructor :accessor destructor :initarg :destructor :type :function :initform nil ; TODO: Better name?
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
   (keymap-scheme :accessor keymap-scheme :initarg :keymap-scheme :type keymap:scheme
                  :initform (define-scheme "root"
                              scheme:cua
                              (list
                               "C-q" #'quit
                               "C-[" #'switch-buffer-previous
                               "C-]" #'switch-buffer-next
                               "C-x b" #'switch-buffer
                               "C-x k" #'delete-buffer ; Emacs' default behaviour is to query.
                               "C-x C-k" #'delete-current-buffer
                               "C-shift-tab" #'switch-buffer-previous
                               "C-tab" #'switch-buffer-next
                               "C-pageup" #'switch-buffer-previous
                               "C-pagedown" #'switch-buffer-next
                               "C-l" #'set-url
                               "M-l" #'set-url-new-buffer
                               "C-m k" #'bookmark-delete
                               "C-t" #'make-buffer-focus
                               "C-m u" #'bookmark-url
                               "f1 v" #'describe-variable
                               "f1 f" #'describe-function
                               "f1 c" #'describe-command
                               "f1 C" #'describe-class
                               "f1 s" #'describe-slot
                               "f1 k" #'describe-key
                               "f1 b" #'describe-bindings
                               "C-o" #'load-file
                               "C-i" #'autofill
                               "M-x" #'execute-command
                               "M-:" #'command-evaluate
                               "C-x 5 2" #'make-window
                               "C-x 5 0" #'delete-current-window
                               "C-x 5 1" #'delete-window
                               "C-/" #'reopen-buffer
                               "C-x C-f" #'open-file)
                              scheme:emacs
                              (list
                               "C-x C-c" #'quit
                               "C-[" #'switch-buffer-previous
                               "C-]" #'switch-buffer-next
                               "C-x b" #'switch-buffer
                               "C-x k" #'delete-buffer ; Emacs' default behaviour is to query.
                               "C-x C-k" #'delete-current-buffer
                               "C-x left" #'switch-buffer-previous
                               "C-x right" #'switch-buffer-next
                               "C-pageup" #'switch-buffer-previous
                               "C-pagedown" #'switch-buffer-next
                               "C-l" #'set-url
                               "M-l" #'set-url-new-buffer
                               "C-m k" #'bookmark-delete
                               "C-t" #'make-buffer-focus
                               "C-m u" #'bookmark-url
                               "C-h v" #'describe-variable
                               "C-h f" #'describe-function
                               "C-h c" #'describe-command
                               "C-h C" #'describe-class
                               "C-h s" #'describe-slot
                               "C-h k" #'describe-key
                               "C-h b" #'describe-bindings
                               "C-o" #'load-file
                               "C-i" #'autofill
                               "M-x" #'execute-command
                               "M-:" #'command-evaluate
                               "C-x 5 2" #'make-window
                               "C-x 5 0" #'delete-current-window
                               "C-x 5 1" #'delete-window
                               "C-/" #'reopen-buffer
                               "C-x C-f" #'open-file)
                              scheme:vi-normal
                              (list
                               "Z Z" #'quit
                               "[" #'switch-buffer-previous
                               "]" #'switch-buffer-next
                               "C-pageup" #'switch-buffer-previous
                               "C-pagedown" #'switch-buffer-next
                               "g b" #'switch-buffer
                               "d" #'delete-buffer
                               "D" #'delete-current-buffer
                               "B" #'make-buffer-focus
                               "o" #'set-url
                               "O" #'set-url-new-buffer
                               "m u" #'bookmark-url
                               "m d" #'bookmark-delete
                               "C-o" #'load-file
                               ;; TODO: Use "f1 *" instead?
                               "C-h v" #'describe-variable
                               "C-h f" #'describe-function
                               "C-h c" #'describe-command
                               "C-h C" #'describe-class
                               "C-h s" #'describe-slot
                               "C-h k" #'describe-key
                               "C-h b" #'describe-bindings
                               ":" #'execute-command
                               "M-:" #'command-evaluate
                               "W" #'make-window
                               "C-w C-w" #'make-window
                               "C-w q" #'delete-current-window
                               "C-w C-q" #'delete-window
                               "u" #'reopen-buffer
                               "C-x C-f" #'open-file)))))

(defmethod object-string ((mode root-mode))
  (symbol-name (class-name (class-of mode))))

(export-always 'find-mode)
(defmethod find-mode ((buffer buffer) mode-symbol)
  "Return the mode corresponding to MODE-SYMBOL in active in BUFFER.
Return nil if mode is not found.  MODE-SYMBOL does not have to be namespaced, it
can be 'web-mode as well as 'next/web-mode:web-mode."
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
  (keymap:get-keymap (keymap-scheme-name (buffer mode))
                     (keymap-scheme mode)))

(defmethod on-signal-notify-uri ((mode root-mode) url)
  (set-window-title (current-window) (buffer mode))
  (print-status)
  url)

(defmethod on-signal-load-committed ((mode root-mode) url)
  url)

(defmethod on-signal-load-finished ((mode root-mode) url)
  ;; TODO: Setting the default zoom level works with pure Javascript, but it
  ;; can only be done after the URL has been loaded which is a bit of a
  ;; kludge.  Instead we could add an FFI endpoint,
  ;; e.g. webkit_web_view_set_zoom_level.
  (unzoom-page :buffer (buffer mode))
  url)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :s-serialization)

(defmethod serializable-slots ((object next::root-mode))
  "Discard keymaps which can be quite verbose."
  (delete 'next::keymap-scheme
          (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots (class-of object)))))
