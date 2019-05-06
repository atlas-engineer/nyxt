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

(defmacro define-command (name (&optional (mode (find-class 'root-mode)) &rest arglist) &body body)
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
       (defmethod ,name ,(cons `(%mode ,mode) arglist)
         ,documentation
         (echo-dismiss (minibuffer *interface*))
         ,@body))))

(defun package-defined-symbols (&optional (package (find-package :next)))
  "Return the list of all symbols interned in this package."
  (let ((symbols))
    (do-symbols (s package symbols)
      (when (eq (symbol-package s) package)
        (push s symbols)))
    symbols))

(defun package-variables ()
  (loop for sym in (package-defined-symbols)
        when (boundp sym)
        collect sym))

(defun package-functions ()
  (loop for sym in (package-defined-symbols)
        when (fboundp sym)
        collect sym))

(defun package-methods ()
  (loop for sym in (package-defined-symbols)
        append (ignore-errors
                (closer-mop:generic-function-methods (symbol-function sym)))))

;; TODO: Find a reliable way to identify commands.  We could use a class (we
;; used to have a COMMAND class) or some sort of symbol properties, or intern
;; the symbol into a special package (see `intern' documentation).
(defun list-commands (&optional mode)
  "List commands.
When MODE is a mode symbol, list only the commands that apply in this mode.
Otherwise list all commands."
  (loop for m in (package-methods)
        for first-specializer = (first (closer-mop:method-specializers m))
        when (and (closer-mop:subclassp first-specializer (find-class 'root-mode))
                  (or (not mode)
                      (closer-mop:subclassp (find-class mode) first-specializer )))
        collect m))

(defun command-complete (input)
  (fuzzy-match input
               (mapcar (lambda (c)
                         (closer-mop:generic-function-name
                          (closer-mop:method-generic-function c)))
                       (list-commands (class-name
                                       (class-of
                                        (mode (active-buffer *interface*))))))
               :accessor-function #'symbol-name))

(define-command execute-command ()
  "Execute a command by name"
  (with-result (command (read-from-minibuffer
                         (minibuffer *interface*)
                         :input-prompt "Execute command:"
                         :completion-function 'command-complete))
    (funcall command (mode (active-buffer *interface*)))))
