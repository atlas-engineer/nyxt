;;; command.lisp --- command definition functions
;;; This file is licensed under license documents/external/LICENSE1.

(in-package :next)

(defclass command ()
  ((sym :accessor sym :initarg :sym)))

(defvar *last-used-commands* nil
  "A list of last used commands by the user. Most recent first.")

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
       (push ',name %%command-list)
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

(defvar %%command-key-bindings (make-hash-table :test #'equal)
  "Internal mapping of (key-binding command).
This is used to memoize the results of `command-key-binding'.")

;; TODO: Define memoized function to get rid of %%command-key-bindings.
;; We need a way to force-reinitialize the memoization when bindings are updated.
;; See http://quickdocs.org/fare-memoization/.
(defun command-key-binding (command-symbol)
  "Return the stringified key bound to COMMAND-symbol."
  (multiple-value-bind (value present-p)
      (gethash command-symbol %%command-key-bindings)
    (if present-p
        value
        (let (key)
          (find-if (lambda (mode)
                     (let ((keymap (getf
                                    (keymap-schemes mode)
                                    (current-keymap-scheme (buffer mode)))))
                       (setf key (first (find-if (lambda (key-command-pairs)
                                                   (eq (rest key-command-pairs) command-symbol))
                                                 (alexandria:hash-table-alist (table keymap)))))))
                   (modes (active-buffer *interface*)))
          (let ((stringified-key (if key (stringify key) nil)))
            (setf (gethash command-symbol %%command-key-bindings) stringified-key)
            stringified-key)))))

(defmethod current-keymap-scheme ((buffer buffer))
  "Return BUFFER's current-keymap-scheme."
  (slot-value buffer 'current-keymap-scheme))

(defmethod (setf current-keymap-scheme) (value (buffer buffer))
  "Set the current-keymap-scheme of BUFFER to VALUE.
This also resets `%%command-key-bindings' so that bindings are properly updated
when displayed in the minibuffer."
  (setf (slot-value buffer 'current-keymap-scheme) value)
  (clrhash %%command-key-bindings))

(defmethod object-string ((command command))
  (let ((binding (command-key-binding (sym command))))
    (if binding
        (format nil "~a (~a)" (sym command) binding)
        (format nil "~a" (sym command)))))

(defun %all-available-commands (interface)
  "List the commands of all modes of this interface."
  (mapcar #'command-symbol
                       (delete-duplicates
                        (loop for mode in (modes (active-buffer interface))
                              append (list-commands (class-name (class-of mode)))))))

;; TODO: Implement a more general "most-recent-access" sorting by using a sort
;; function in fuzzy-match and by adding a "access-time" slot to commands.
(defun all-available-commands (interface)
  "List the Next commands for that interface. Re-order them a bit more user-friendly than a mere listing.
Currently, we list the last used commands first."
  (let ((commands (%all-available-commands interface)))
    (mapcar (lambda (c) (make-instance 'command :sym c))
            (remove-duplicates (append *last-used-commands* commands) :from-end t))))

(defun command-complete (input)
  (fuzzy-match input
               (all-available-commands *interface*)))

(define-command execute-command ()
  "Execute a command by name."
  (with-result (command (read-from-minibuffer
                         (minibuffer *interface*)
                         :input-prompt "Execute command:"
                         :completion-function 'command-complete))
    (let ((mode (find-if (lambda (mode)
                           (member (sym command) (mapcar #'command-symbol
                                                         (list-commands (class-name (class-of mode))))))
                         (modes (active-buffer *interface*)))))
      (push (sym command) *last-used-commands*)
      (funcall (sym command) mode))))
