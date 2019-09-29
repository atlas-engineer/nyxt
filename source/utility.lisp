;;; utility.lisp --- utility classes and functions
;; Split this file into smaller ones when it becomes relevant.

(in-package :next)
(annot:enable-annot-syntax)

@export
(defmethod object-string ((object t))
  (princ-to-string object))

(define-command start-swank (&optional (swank-port *swank-port*))
  "Start a Swank server that can be connected to, for instance, in Emacs via
SLIME."
  (swank:create-server :port swank-port :dont-close t))

@export
(defun xdg-data-home (&optional (file-name ""))
  "Return XDG_DATA_HOME as per XDG directory specification.
FILE-NAME is appended to the result."
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-data-home))))

@export
(defun xdg-config-home (&optional (file-name ""))
  "Return XDG_CONFIG_HOME as per XDG directory specification.
FILE-NAME is appended to the result."
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "next"))
    (uiop:xdg-config-home))))

(defun ensure-parent-exists (path)
  "Create parent directories of PATH if they don't exist and return PATH."
  (ensure-directories-exist (directory-namestring path))
  path)

(defun ensure-file-exists (path &optional (init-function))
  "Create file pointed by PATH if it does not exists.  Return PATH's truename.
When non-nil, INIT-FUNCTION is used to create the file, else the file will be empty."
  (unless (uiop:file-exists-p path)
    (if init-function
        (funcall init-function path)
        (close (open (ensure-parent-exists path) :direction :probe :if-does-not-exist :create))))
  (truename path))

(defun find-slot (class slot-name)
  "CLASS can be a symbol or a class."
  (when (symbolp class)
    (setf class (closer-mop:ensure-finalized (find-class class))))
  (find-if (lambda (slot)
             (eq (closer-mop:slot-definition-name slot)
                 slot-name))
           (closer-mop:class-slots class)))

@export
(defun get-default (class-name slot-name)
  "Get default value of slot SLOT-NAME from class CLASS-NAME.
The second value is the initfunction."
  (let* ((class (closer-mop:ensure-finalized (find-class class-name)))
         (slot (find-slot class slot-name))
         (value (closer-mop:slot-definition-initform slot)))
    ;; When querying quoted lists, the return value of slot-definition-initform
    ;; is quoted.  For lists declared with LIST, the return value is a list starting with symbol LIST.
    ;; In those cases, we eval it here so that the caller does not have to do it.
    ;; Besides, when the slot value is updated with SETF, the list is stored
    ;; unquoted / without LIST.  By evaluating here, we make sure that all calls to GET-DEFAULT
    ;; have consistent return types.  WARNING: This could be limitating if slot
    ;; was meant to actually store a quoted list.  Should this happen, we would
    ;; have to take some provision.
    (if (and (listp value)
             (or
              (eq 'quote (first value))
              (eq 'list (first value))))
        (eval value)
        value)))

@export
(defun (setf get-default) (value class-name slot-name)
  "Set default value of SLOT-NAME from CLASS-NAME.
Return VALUE.

This only changes the default value for future instances.  Existing instances
won't be affected."
  ;; Warning: This is quite subtle: the :initform and :initfunction are tightly
  ;; coupled, it seems that both must be changed together.  We need to change
  ;; the class-slots and not the class-direct-slots.  TODO: Explain why.
  (log:warn "The (setf (get-default ...)) and (add-to-default-list ...) forms are deprecated.") ; TODO: Remove after 1.3.3.
  (let* ((class (closer-mop:ensure-finalized (find-class class-name)))
         (slot (find-slot class slot-name)))
    (setf (closer-mop:slot-definition-initfunction slot) (lambda () value))
    (setf (closer-mop:slot-definition-initform slot) value)))

(defun add-to-default-list (value class-name slot-name)
  "Add VALUE to the list SLOT-NAME from CLASS-NAME.
If VALUE is already present, move it to the head of the list.

This only changes the default value for future instances.  Existing instances
won't be affected."
  (setf (get-default class-name slot-name)
        (remove-duplicates (cons value
                                 (get-default class-name slot-name))
                           :from-end t)))

@export
(defun member-string (string list)
  "Return the tail of LIST beginning whose first element is STRING."
  (check-type string string)
  (member string list :test #'string=))

;; This is mostly inspired by Emacs 26.2.
@export
(defun file-size-human-readable (file-size &optional flavor)
  "Produce a string showing FILE-SIZE in human-readable form.

Optional second argument FLAVOR controls the units and the display format:

 If FLAVOR is nil or omitted, each kilobyte is 1024 bytes and the produced
    suffixes are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `si', each kilobyte is 1000 bytes and the produced suffixes
    are \"k\", \"M\", \"G\", \"T\", etc.
 If FLAVOR is `iec', each kilobyte is 1024 bytes and the produced suffixes
    are \"KiB\", \"MiB\", \"GiB\", \"TiB\", etc."
  (let ((power (if (or (null flavor) (eq flavor 'iec))
                   1024.0
                   1000.0))
        (post-fixes
          ;; none, kilo, mega, giga, tera, peta, exa, zetta, yotta
          (list "" "k" "M" "G" "T" "P" "E" "Z" "Y"))
        (format-string "~d~a~a"))
    (loop while (and (>= file-size power) (rest post-fixes))
          do (setf file-size (/ file-size power)
                   post-fixes (rest post-fixes)))
    (if (> (abs (- file-size (round file-size))) 0.05)
        (setf format-string "~,1f~a~a")
        (setf file-size (round file-size)))
    (format nil format-string
            file-size
            (if (and (eq flavor 'iec) (string= (first post-fixes) "k"))
                "K"
                (first post-fixes))
            (cond
              ((and (eq flavor 'iec)
                    (string= (first post-fixes) ""))
               "B")
              ((eq flavor 'iec) "iB")
              (t "")))))

(declaim (ftype (function (fixnum &optional fixnum)) kill-program))
(defun kill-program (pid &optional (signal 15))
  (handler-case (uiop:run-program
                 (list "kill" (format nil "-~a" signal)
                       (write-to-string pid)))
    (error ()
      (log:error "Process with PID ~a is not running" pid))))

(declaim (ftype (function (string &rest string)) run-program-to-string))
(defun run-program-to-string (program &rest args)
  "Run PROGRAM over ARGS and return the its standard output."
  (handler-case
      (multiple-value-bind (output error code)
          (uiop:run-program (cons program args)
                            :output '(:string :stripped t)
                            :error-output '(:string :stripped t)
                            :ignore-error-status t)
        (if (not (= 0 code))
            (error "~a error: ~a" program error)
            output))
    (error ()
      (error "~a not found" program))))

;; TODO: Backport upstream?  See https://github.com/scymtym/architecture.hooks/issues/2.
;; TODO: For now the user has no way to choose between `hooks:run-hook' and
;; `next:run-composed-hook'.
(defun run-composed-hook (hook &rest args)
  "Compose all handlers in HOOK and call the resulting function over ARGS."
  (if hook
      (apply (apply #'alexandria:compose hook) args)
      ;; We return values so that this is equivalent to #'identity when there is
      ;; no hook.
      (apply #'values args)))
