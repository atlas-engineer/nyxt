;;; command.lisp --- command definition functions
;;; This file is licensed under license documents/external/LICENSE1.

(in-package :next)
(annot:enable-annot-syntax)

;; We need a `command' class for multiple reasons:
;; - Identify commands uniquely (although being a member of `%%command-list' is enough).
;;
;; - Customize minibuffer display value with `object-string'.
;;
;; - Access-time: This is useful to sort command by the time they were last
;;   called.  The only way to do this is to persist the command instances.
(defclass command ()
  ((sym :accessor sym :initarg :sym :type symbol :initform nil) ; TODO: Make constructor?
   (pkg :accessor pkg :initarg :pkg :type package :initform nil)
   (sexp :accessor sexp :initarg :sexp :type sexp :initform nil)
   (access-time :accessor access-time :type integer :initform 0
                :documentation "Last time this command was called from minibuffer.
This can be used to order the commands.")))

(define-condition documentation-style-warning (style-warning)
  ((name :initarg :name :reader name)
   (subject-type :initarg :subject-type :reader subject-type))
  (:report
   (lambda (condition stream)
     (format stream
             "~:(~A~) ~A doesn't have a documentation string"
             (subject-type condition)
             (name condition)))))

(define-condition command-documentation-style-warning
    (documentation-style-warning)
  ((subject-type :initform 'command)))

@export
(defmacro define-command (name (&rest arglist) &body body)
  "Define new command NAME.
ARGLIST must be a list of optional arguments.
This macro also define two hooks, NAME-before-hook and NAME-after-hook.
Regardless of the hook, the command returns the last expression of BODY."
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body))
        (before-hook (intern (str:concat (symbol-name name) "-BEFORE-HOOK")))
        (after-hook (intern (str:concat (symbol-name name) "-AFTER-HOOK"))))
    `(progn
       @export
       (defparameter ,before-hook (next-hooks:make-hook-void))
       @export
       (defparameter ,after-hook (next-hooks:make-hook-void))
       (unless (find-if (lambda (c) (and (eq (sym c) ',name)
                                         (eq (pkg c) *package*)))
                        %%command-list)
         (push (make-instance 'command :sym ',name :pkg *package* :sexp '(progn ,@body)) %%command-list))
       @export
       ;; We use defun to define the command instead of storing a lambda because we want
       ;; to be able to call the foo command from Lisp with (FOO ...).
       (defun ,name ,arglist
         ,documentation
         (handler-case
             (progn
               (next-hooks:run-hook ,before-hook)
               (log:debug "Calling command ~a." ',name)
               ;; TODO: How can we print the arglist as well?
               ;; (log:debug "Calling command (~a ~a)." ',name (list ,@arglist))
               (prog1
                   (progn
                     ,@body)
                 (next-hooks:run-hook ,after-hook)))
           (next-condition (c)
             (format t "~s" c)))))))

;; TODO: Update define-deprecated-command
(defmacro define-deprecated-command (name (&rest arglist) &body body)
  "Define NAME, a deprecated command.
This is just like a command.  It's recommended to explain why the function is
deprecated and by what in the docstring."
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body)))
    `(progn
       (define-command ,name ,arglist
         ,documentation
         (progn
           ;; TODO: Implement `warn'.
           (echo-warning "~a is deprecated." ',name)
           ,@body)))))

(defun package-defined-symbols (&optional (package (find-package :next)))
  "Return the list of all symbols interned in this package."
  (let ((symbols))
    (do-symbols (s package symbols)
      (when (eq (symbol-package s) package)
        (push s symbols)))
    symbols))

(defun package-variables ()
  (remove-if (complement #'boundp) (package-defined-symbols)))

(defun package-functions ()             ; TODO: Unused.  Remove?
  (remove-if (complement #'fboundp) (package-defined-symbols)))

(defun package-methods ()               ; TODO: Unused.  Remove?
  (loop for sym in (package-defined-symbols)
        append (ignore-errors
                (closer-mop:generic-function-methods (symbol-function sym)))))

(defmethod mode-toggler-p ((command command))
  "Return non-nil if COMMAND is a mode toggler.
A mode toggler is a command of the same name as its associated mode."
  (ignore-errors
   (closer-mop:subclassp (find-class (sym command) nil)
                         (find-class 'root-mode))))

(defun list-commands (&rest mode-symbols)
  "List commands.

Commands are instances of the `command' class.  When MODE-SYMBOLS are
provided, list only the commands that belong to this mode.
If 'ROOT-MODE is in MODE-SYMBOLS, mode togglers are included.

Otherwise list all commands."
  (if mode-symbols
      (let ((list-togglers-p (member 'root-mode mode-symbols)))
        (remove-if (lambda (c)
                     (and (notany (lambda (m)
                                    (match m
                                      ('root-mode (eq (pkg c) (find-package :next)))
                                      (_ (eq (pkg c)
                                             (pkg (mode-command m))))))
                                  mode-symbols)
                          (or (not list-togglers-p)
                              (not (mode-toggler-p c)))))
                   %%command-list))
      %%command-list))

(defmethod object-string ((command command))
  (str:downcase (sym command)))

(defmethod object-display ((command command))
  ;; Use `last-active-window' for speed, or else the minibuffer will stutter
  ;; because of the RPC calls.
  (let* ((buffer (active-buffer (last-active-window *browser*)))
         (scheme (current-keymap-scheme buffer))
         (bindings '()))
    (loop for mode in (modes buffer)
          for scheme-keymap = (getf (keymap-schemes mode) scheme)
          when scheme-keymap
            do (setf bindings (mapcar #'first
                                      (keymap:symbol-keys (command-function command) scheme-keymap)))
          when (not (null bindings))
            return bindings)
    (format nil "~a~a~a"
            (str:downcase (sym command))
            (if bindings
                (format nil " (~{~a~^, ~})" bindings)
                "")
            (match (object-string (pkg command))
              ((or "" "next" "next-user") "")
              (a (format nil " [~a]" a))))))

(defun command-completion-filter (input)
  (fuzzy-match input
               (sort (apply #'list-commands (mapcar (alexandria:compose #'class-name #'class-of)
                                                    (modes (current-buffer))))
                     (lambda (c1 c2)
                       (> (access-time c1) (access-time c2))))))

(defmethod command-function ((command command))
  "Return the function associated to COMMAND.
This function can be `funcall'ed."
  (symbol-function (find-symbol
                    (string (sym command))
                    (pkg command))))

(defmethod run ((command command) &rest args)
  "Run COMMAND over ARGS."
  (apply #'funcall-safely (command-function command) args))

(define-command execute-command ()
  "Execute a command by name."
  (with-result (command (read-from-minibuffer
                         (make-minibuffer
                          :input-prompt "Execute command"
                          :completion-function 'command-completion-filter
                          :show-completion-count nil)))
    (setf (access-time command) (get-internal-real-time))
    (run command)))

(defclass hook-description ()
  ((name :accessor name :initarg :name
         :initform ""
         :type string
         :documentation "The hook name.")
   (value :accessor value :initarg :value
          :documentation "The hook value.")))

(defmethod object-string ((hook-desc hook-description))
  (name hook-desc))

(defmethod object-string ((handler next-hooks:handler))
  (str:downcase (next-hooks:name handler)))

(defun hook-completion-filter (input)
  (flet ((list-hooks (object)
           (mapcar (lambda (hook)
                     (make-instance 'hook-description
                                    :name (str:downcase (closer-mop:slot-definition-name hook))
                                    :value (funcall (symbol-function (closer-mop:slot-definition-name hook))
                                                    object)))
                   (delete-if-not (lambda (s)
                                    (str:ends-with-p "-hook"
                                                     (str:downcase (closer-mop:slot-definition-name s))))
                                  (closer-mop:class-slots (class-of object))))))
    (let ((window-hooks
            (list-hooks (last-active-window *browser*)))
          (buffer-hooks (list-hooks (current-buffer)))
          (browser-hooks (list-hooks *browser*)))
      (fuzzy-match input
                   (append window-hooks
                           buffer-hooks
                           browser-hooks)))))

(defun handler-completion-filter (hook)
  (lambda (input)
    (fuzzy-match input
                 (next-hooks:handlers hook))))

(defun disabled-handler-completion-filter (hook)
  (lambda (input)
    (fuzzy-match input
                 (next-hooks:disabled-handlers hook))))

(define-command disable-hook-handler ()
  "Remove handler(s) from a hook."
  (with-result (hook-desc (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Hook where to disable handler"
                            :completion-function 'hook-completion-filter)))
    (with-result (handler
                  (read-from-minibuffer
                   (make-minibuffer
                    :input-prompt (format nil "Disable handler from ~a" (name hook-desc))
                    :completion-function (handler-completion-filter (value hook-desc)))))
      (next-hooks:disable-hook (value hook-desc) handler))))

(define-command enable-hook-handler ()
  "Remove handler(s) from a hook."
  (with-result (hook-desc (read-from-minibuffer
                           (make-minibuffer
                            :input-prompt "Hook where to enable handler"
                            :completion-function 'hook-completion-filter)))
    (with-result (handler
                  (read-from-minibuffer
                   (make-minibuffer
                    :input-prompt (format nil "Enable handler from ~a" (name hook-desc))
                    :completion-function (disabled-handler-completion-filter (value hook-desc)))))
      (next-hooks:enable-hook (value hook-desc) handler))))
