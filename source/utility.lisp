;;; utility.lisp --- utility classes and functions
;; Split this file into smaller ones when it becomes relevant.

(in-package :next)

(serapeum:export-always 'object-string)
(defmethod object-string ((object t))
  (princ-to-string object))

(defmethod object-display ((object t))
  "Text shown by completion candidates in the minibuffer."
  (object-string object))

(defmethod object-string ((package package))
  (if (eq (package-name package) (find-package :next))
      ""
      (str:replace-all "next/" "" (str:downcase (package-name package)))))

(define-command start-swank (&optional (swank-port *swank-port*))
  "Start a Swank server that can be connected to, for instance, in Emacs via
SLIME."
  (swank:create-server :port swank-port :dont-close t))

(defun ensure-parent-exists (path)
  "Create parent directories of PATH if they don't exist and return PATH."
  (ensure-directories-exist (directory-namestring path))
  path)

(serapeum:export-always 'member-string)
(defun member-string (string list)
  "Return the tail of LIST beginning whose first element is STRING."
  (check-type string string)
  (member string list :test #'string=))

(serapeum:export-always 'notify)
(defun notify (msg)
  "Echo this message and display it with a desktop notification system (notify-send on linux, terminal-notifier on macOs)."
  (echo-safe msg)
  (ignore-errors
    (uiop:launch-program
     #+linux
     (list "notify-send" msg)
     #+darwin
     (list "terminal-notifier" "-title" "Next" "-message" msg))))

(serapeum:export-always 'launch-and-notify)
(defun launch-and-notify (command &key (success-msg "Command succeded.") (error-msg "Command failed."))
  "Run this program asynchronously and notify when it is finished."
  (bt:make-thread
   (lambda ()
     (let ((exit-code (uiop:wait-process
                       (uiop:launch-program command))))
       (notify (if (zerop exit-code) success-msg error-msg))))))

(defmethod write-output-to-log ((browser browser))
  "Set the *standard-output* and *error-output* to write to a log file."
  (values
   (setf *standard-output*
         (open (standard-output-path browser) :direction :output
                                                :if-does-not-exist :create :if-exists :append))
   (setf *error-output*
         (open (error-output-path browser) :direction :output :if-does-not-exist :create
                                             :if-exists :append))))

(defun funcall-safely (f &rest args)
  "Like `funcall' except that if `*keep-alive*' is nil (e.g. the program is run
from a binary) then any condition is logged instead of triggering the debugger."
  (handler-case
      (apply f args)
    (error (c)
      (if *keep-alive*
          (error c)
          ;; TODO: Echo this in a status bar or else it won't be seen if it
          ;; happens when the minibuffer is up.
          (log:error "Error in ~a: ~a" f c))
      nil)))

(sera:export-always '%paste)
(define-parenscript %paste ((input-text (ring-insert-clipboard (clipboard-ring *browser*))))
  (let* ((active-element (ps:chain document active-element))
         (start-position (ps:chain active-element selection-start))
         (end-position (ps:chain active-element selection-end)))
    (setf (ps:chain active-element value)
          (+ (ps:chain active-element value (substring 0 start-position))
             (ps:lisp input-text)
             (ps:chain active-element value
                       (substring end-position
                                  (ps:chain active-element value length)))))))

(define-parenscript %print-buffer ()
  (print))

(sera:export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (%print-buffer))

(sera:export-always '%copy)
(define-parenscript %copy ()
  "Return selected text from javascript."
  (ps:chain window (get-selection) (to-string)))

(sera:export-always 'set-renderer)
(defun set-renderer (&optional renderer)
  "Set the renderer to the RENDERER if supplied, otherwise, check for
   the existence of classes that define renderers."
  (if renderer
      (if (closer-mop:subclassp (find-class renderer) 'browser)
          (setf *renderer-class* (intern (str:replace-all "-BROWSER" "" (symbol-name renderer))))
          (error "RENDERER must extend BROWSER class."))
      (let ((found-renderer-class (class-name (first (mopu:subclasses 'browser)))))
        (setf *renderer-class*
              (intern (str:replace-all "-BROWSER" "" (symbol-name found-renderer-class)))))))

(serapeum:export-always '%slot-default)
(serapeum:export-always 'define-configuration)
(defmacro define-configuration (super &body slots)
  "Helper macro to customize class slots.
It generates a user-SUPER subclass of SUPER.
It binds `*SUPER-class*' to this newly generated class.

The `%slot-default' variable is replaced by the slot initform.

Example:

\(define-configuration buffer
  (default-modes (append '(vi-normal-mode) %slot-default)))"
  (let* ((name (intern (str:concat "USER-" (symbol-name super))))
         (configured-class (intern (str:concat "*" (symbol-name super) "-CLASS*")))
         (super (intern (str:concat (symbol-name *renderer-class*) "-" (symbol-name super)))))
    `(progn
       (defclass ,name (,super)
         ,(loop with super-class = (closer-mop:ensure-finalized (find-class super))
                for slot in (car slots)
                for known-slot? = (find (car slot) (mopu:slot-names (closer-mop:ensure-finalized (find-class super))))
                for initform = (and known-slot?
                                    (getf (mopu:slot-properties super-class (car slot))
                                          :initform))
                if known-slot?
                  collect (list (car slot) :initform `(funcall (lambda (%slot-default)
                                                                 (declare (ignorable %slot-default))
                                                                 ,(cadr slot))
                                                               ,initform))
                else do
                  (log:warn "Undefined slot ~a in ~a" (car slot) super)))
       (setf ,configured-class ',name))))
