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
  ((sym :accessor sym :initarg :sym :type :symbol :initform nil) ; TODO: Make constructor?
   (pkg :accessor pkg :initarg :pkg :type :package :initform nil)
   (access-time :accessor access-time :type :integer :initform 0
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
       (defparameter ,before-hook '())
       @export
       (defparameter ,after-hook '())
       (unless (find-if (lambda (c) (and (eq (sym c) ',name)
                                         (eq (pkg c) *package*)))
                        %%command-list)
         (push (make-instance 'command :sym ',name :pkg *package*) %%command-list))
       @export
       (defun ,name ,arglist
         ,documentation
         (handler-case
             (progn
               (hooks:run-hook ',before-hook)
               (log:debug "Calling command ~a." ',name)
               ;; TODO: How can we print the arglist as well?
               ;; (log:debug "Calling command (~a ~a)." ',name (list ,@arglist))
               (prog1
                   (progn
                     ,@body)
                 (hooks:run-hook ',after-hook)))
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
  ;; Use `last-active-window' for speed, or else the minibuffer will stutter
  ;; because of the RPC calls.
  (let* ((buffer (active-buffer (last-active-window *interface*)))
         (scheme (current-keymap-scheme buffer))
         (bindings '()))
    (loop for mode in (modes buffer)
          for scheme-keymap = (getf (keymap-schemes mode) scheme)
          when scheme-keymap
            do (let ((table (table scheme-keymap)))
               (maphash (lambda (binding c)
                          (when (eq c (command-function command))
                            (push (stringify binding) bindings)))
                        table))
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
  "Return the function associate to COMMAND.
This function can be `funcall'ed."
  (symbol-function (find-symbol
                    (string (sym command))
                    (pkg command))))

(defmethod run ((command command) &rest args)
  "Run COMMAND over ARGS."
  (apply (command-function command)
         args))

(define-command execute-command ()
  "Execute a command by name."
  (with-result (command (read-from-minibuffer
                         (make-instance 'minibuffer
                                        :input-prompt "Execute command:"
                                        :completion-function 'command-completion-filter)))
    (setf (access-time command) (get-internal-real-time))
    (run command)))
