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
       (unless (find-if (lambda (c) (and (eq (sym c) ',name)
                                         (eq (mode c) ',mode)))
                        %%command-list)
         (push (make-instance 'command :sym ',name :mode ',mode) %%command-list))
       (defmethod ,name ,(cons `(,mode ,mode) arglist)
         ,documentation
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

(defun package-functions ()             ; TODO: Unused.  Remove?
  (remove-if-not #'fboundp (package-defined-symbols)))

(defun package-methods ()               ; TODO: Unused.  Remove?
  (loop for sym in (package-defined-symbols)
        append (ignore-errors
                (closer-mop:generic-function-methods (symbol-function sym)))))

(defun list-commands (&rest modes)
  "List commands.
Commands are instance of the `command' class.  When MODES are provided (as mode
symbols), list only the commands that apply to this mode.  Otherwise list all
commands."
  (if modes
      (delete-duplicates (loop for command in %%command-list
                               when (some
                                     (lambda (m)
                                       (closer-mop:subclassp (find-class (mode command))
                                                             (find-class m)))
                                     modes)
                                 collect command))
      %%command-list))

(defmethod object-string ((command command))
  ;; Use `last-active-window' for speed, or else the minibuffer will stutter
  ;; because of the RPC calls.
  (let* ((scheme (current-keymap-scheme (active-buffer (last-active-window *interface*))))
         (mode (find-mode (active-buffer (last-active-window *interface*))
                          (mode command)))
         (bindings '()))
    (when mode
      (let ((table (table (getf (keymap-schemes mode) scheme))))
        (maphash (lambda (binding c)
                   (when (eq c (sym command))
                     (push (stringify binding) bindings)))
                 table)))
    (if bindings
        (format nil "~a (~{~a~^, ~})" (sym command) bindings)
        (format nil "~a" (sym command)))))

(defun command-complete (input)
  (fuzzy-match input
               (sort (apply #'list-commands (mapcar (alexandria:compose #'class-name #'class-of)
                                                    (modes (active-buffer *interface*))))
                     (lambda (c1 c2)
                       (> (access-time c1) (access-time c2))))))

(define-command execute-command ()
  "Execute a command by name."
  (with-result (command (read-from-minibuffer
                         (minibuffer *interface*)
                         :input-prompt "Execute command:"
                         :completion-function 'command-complete))
    (setf (access-time command) (get-internal-real-time))
    (funcall (sym command) (find-mode (active-buffer *interface*) (mode command)))))
