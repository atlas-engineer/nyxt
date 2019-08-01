;;; command.lisp --- command definition functions
;;; This file is licensed under license documents/external/LICENSE1.

(in-package :next)

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

;; TODO: Find a better way to uniquely identidy commands from mode methods.
;; What about symbol properties?  We could use:
;;
;; (setf (get name 'commandp) t)
;;
;; But that doesn't seem to work properly, some commands need to be evaluated
;; twice before they appear in the list.  We could use a class (we used to have
;; a COMMAND class) or intern the symbol into a special package (see `intern'
;; documentation).
(defvar %%command-list ()
  "The list of known commands, for internal use only.")

(defmacro define-command (name (&optional (mode 'root-mode) &rest arglist) &body body)
  "Define new command NAME.
MODE most be a subclass of root-mode.
ARGLIST must be a list of optional arguments."
  (let ((documentation (if (stringp (first body))
                           (first body)
                           (warn (make-condition
                                  'command-documentation-style-warning
                                  :name name))))
        (body (if (stringp (first body))
                  (rest body)
                  body)))
    `(progn
       (push ',name %%command-list)
       (defmethod ,name ,(cons `(,mode ,mode) arglist)
         ,documentation
         (when *interface*
           (echo-dismiss (minibuffer *interface*)))
         ,@body))))

(defmacro define-deprecated-command (name (&optional (mode 'root-mode) &rest arglist) &body body)
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
       (define-command ,name (,mode ,@arglist)
         ,documentation
         (progn
           ;; TODO: Implement `warn'.
           (echo "Warning: ~a is deprecated." ',name)
           (log:warn "Deprecated command ~a" ',name)
           ,@body)))))

(defun package-defined-symbols (&optional (package (find-package :next)))
  "Return the list of all symbols interned in this package."
  (let ((symbols))
    (do-symbols (s package symbols)
      (when (eq (symbol-package s) package)
        (push s symbols)))
    symbols))

(defun package-variables ()
  (remove-if-not #'boundp (package-defined-symbols)))

(defun package-functions ()
  (remove-if-not #'fboundp (package-defined-symbols)))

(defun package-methods ()
  (loop for sym in (package-defined-symbols)
        append (ignore-errors
                (closer-mop:generic-function-methods (symbol-function sym)))))

(defun list-commands (&optional mode)
  "List commands.
A command is a mode method that is in the %%command-list.
When MODE is a mode symbol, list only the commands that apply in this mode.
Otherwise list all commands."
  (loop for m in (package-methods)
        for first-specializer = (first (closer-mop:method-specializers m))
        when (and (member (command-symbol m)
                          %%command-list)
                  (closer-mop:subclassp first-specializer (find-class 'root-mode))
                  (or (not mode)
                      (closer-mop:subclassp (find-class mode) first-specializer)))
          collect m))

(defun command-symbol (command)
  "Return the symbol of a command."
  (closer-mop:generic-function-name
   (closer-mop:method-generic-function command)))

(defun command-complete (input)
  (fuzzy-match input
               (mapcar #'command-symbol
                       (delete-duplicates
                        (loop for mode in (modes (active-buffer *interface*))
                              append (list-commands (class-name (class-of mode))))))))

(define-command execute-command ()
  "Execute a command by name."
  (with-result (command (read-from-minibuffer
                         (minibuffer *interface*)
                         :input-prompt "Execute command:"
                         :completion-function 'command-complete))
    (let ((mode (find-if (lambda (mode)
                           (member command (mapcar #'command-symbol
                                                   (list-commands (class-name (class-of mode))))))
                         (modes (active-buffer *interface*)))))
      (funcall command mode))))
