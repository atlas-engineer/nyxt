(in-package :nyxt)

;; TODO: Split this file into smaller ones when it becomes relevant.

(export-always 'object-string)
(defmethod object-string ((object t))
  (princ-to-string object))

(export-always 'object-display)
(defmethod object-display ((object t))
  "Text shown by completion candidates in the minibuffer."
  (if (and (mopu:slot-names (class-of object))
           (find (package-name (symbol-package (class-name (class-of object))))
                 (nyxt-packages)
                 :test #'string=))
      (format nil "~a:~%~%~a"
              object
              (format-object object))
      (object-string object)))

(defmethod object-string ((package package))
  (if (eq (package-name package) (find-package :nyxt))
      ""
      (str:replace-all "nyxt/" "" (str:downcase (package-name package)))))

(defun slot-definitions (object)
  (mapcar (lambda (slot-name)
            (list
             slot-name
             (let ((value (slot-value object slot-name)))
               (if (listp value)
                   (trim-list value)
                   value))))
          (mopu:slot-names object)))

(defun format-object (object)
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (pair)
                    (format nil "~a: ~a"
                            (first pair)
                            ;; TODO: We could call object-display and recurse
                            ;; here, but this would require us to implement some
                            ;; form of indentation, lest it becomes unreadable.
                            ;; Better: Make it clickable just like the SLIME inspector.
                            (object-string (second pair))))
                  (slot-definitions object))))

(define-command start-swank (&optional (swank-port *swank-port*))
  "Start a Swank server that can be connected to, for instance, in
Emacs via SLIME.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
  (swank:create-server :port swank-port :dont-close t)
  (echo "Swank server started at port ~a" swank-port))

(export-always 'member-string)
(defun member-string (string list)
  "Return the tail of LIST beginning whose first element is STRING."
  (check-type string string)
  (member string list :test #'string=))

(export-always 'notify)
(defun notify (msg)
  "Echo this message and display it with a desktop notification system (notify-send on linux, terminal-notifier on macOs)."
  (echo-warning msg)
  (ignore-errors
    (uiop:launch-program
     #+linux
     (list "notify-send" msg)
     #+darwin
     (list "terminal-notifier" "-title" "Nyxt" "-message" msg))))

(export-always 'launch-and-notify)
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
         (open (expand-path (standard-output-path browser))
               :direction :output
               :if-does-not-exist :create
               :if-exists :append))
   (setf *error-output*
         (open (expand-path (error-output-path browser))
               :direction :output
               :if-does-not-exist :create
               :if-exists :append))))

(export-always 'funcall-safely)
(defun funcall-safely (f &rest args)
  "Like `funcall' except that if `*keep-alive*' is nil (e.g. the program is run
from a binary) then any condition is logged instead of triggering the debugger."
  (if *keep-alive*
      (apply f args)
      (handler-case
          (apply f args)
        (error (c)
          (log:error "In ~a: ~a" f c)
          nil))))

(declaim (ftype (function (containers:ring-buffer-reverse) string) ring-insert-clipboard))
(export-always 'ring-insert-clipboard)
(defun ring-insert-clipboard (ring)
  "Check if clipboard-content is most recent entry in RING.
If not, insert clipboard-content into RING.
Return most recent entry in RING."
  (let ((clipboard-content (trivial-clipboard:text)))
    (unless (string= clipboard-content (unless (containers:empty-p ring)
                                         (containers:first-item ring)))
      (containers:insert-item ring clipboard-content)))
  (string (containers:first-item ring)))

(export-always '%paste)
(define-parenscript %paste (&key (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
  (let ((active-element (ps:chain document active-element))
        (tag (ps:chain document active-element tag-name)))
    (when (or (string= tag "INPUT")
              (string= tag "TEXTAREA"))
      (let ((start-position (ps:chain active-element selection-start))
            (end-position (ps:chain active-element selection-end)))
        (setf (ps:chain active-element value)
              (+ (ps:chain active-element value (substring 0 start-position))
                 (ps:lisp input-text)
                 (ps:chain active-element value
                           (substring end-position
                                      (ps:chain active-element value length)))))))))

(define-parenscript document-get-body (&key (limit 100000))
  (ps:chain document body |innerHTML| (slice 0 (ps:lisp limit))))

(define-parenscript %print-buffer ()
  (print))

(export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (%print-buffer))

(export-always '%copy)
(define-parenscript %copy ()
  "Return selected text from javascript."
  (ps:chain window (get-selection) (to-string)))

(export-always 'set-renderer)
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

(export-always '%slot-default)
(export-always 'define-configuration)
(defmacro define-configuration (super &body slots)
  "Helper macro to customize class slots.
It generates a user-SUPER subclass of SUPER.
It binds `*SUPER-class*' to this newly generated class.

Classes can be modes or a core class like `browser', `buffer', `minibuffer',
`window'.

The `%slot-default' variable is replaced by the slot initform.

Example that sets some defaults for all buffers:

\(define-configuration buffer
  ((status-buffer-height 24)
   (default-modes (append '(vi-normal-mode) %slot-default))))

In the above, `%slot-default' will be substituted with the default value of
`default-modes'.

To discover the default value of a slot or all slots of a class, use the
`describe-slot' or `describe-class' commands respectively.

Since modes are classes with class variables (the `*MODE-class*'), the same applies.

Example to get the `blocker-mode' command to use a new default hostlists:

\(define-configuration nyxt/blocker-mode:blocker-mode
  ((nyxt/blocker-mode:hostlists (append (list *my-blocked-hosts*) %slot-default))))

Since the above binds `nyxt/blocker-mode:*blocker-mode-class*' to
`user-blocker-mode', the `blocker-mode' command now toggles the new
`user-blocker-mode' instead of `blocker-mode'."

  (let* ((name (intern (str:concat "USER-" (symbol-name super))))
         (configured-class)
         (super-variant (let ((super-variant (intern (str:concat (symbol-name *renderer-class*) "-" (symbol-name super)))))
                          ;; SUPER may not have a renderer variant, e.g. modes don't.
                          (if (find-class super-variant nil)
                              super-variant
                              super))))
    (unless (find-class super-variant nil)
      (error "define-configuration argument ~a is not a known class." super))
    (setf configured-class (intern (str:concat "*" (symbol-name super) "-CLASS*") (symbol-package super-variant)))
    `(progn
       (defclass ,name (,super-variant)
         ,(loop with super-class = (closer-mop:ensure-finalized (find-class super-variant))
                for slot in (car slots)
                for known-slot? = (find (car slot) (mopu:slot-names super-class))
                for initform = (and known-slot?
                                    (getf (mopu:slot-properties super-class (car slot))
                                          :initform))
                if known-slot?
                  collect (list (car slot) :initform `(funcall (lambda (%slot-default)
                                                                 (declare (ignorable %slot-default))
                                                                 ,(cadr slot))
                                                               ,initform))
                else do
                  (log:warn "Undefined slot ~a in ~a" (car slot) super-variant)))
       (setf ,configured-class ',name))))

(export-always 'load-system)
(defun load-system (system)
  "Load Common Lisp SYSTEM.
Use Quicklisp if possible.
Return NIL if system could not be loaded and return the condition as a second value.

Initialization file use case:

(when (load-system :foo)
  (defun function-if-foo-is-found () ...))"
  (ignore-errors
   #+quicklisp
   (ql:quickload system :silent t)
   #-quicklisp
   (asdf:load-system system)))

(defun make-ring (&key (size 1000))
  "Return a new ring buffer."
  (containers:make-ring-buffer size :last-in-first-out))

(export-always 'copy-to-clipboard)
(defun copy-to-clipboard (input)
  "Save INPUT text to clipboard, and ring."
  (containers:insert-item (clipboard-ring *browser*) (trivial-clipboard:text input)))

(export-always 'trim-list)
(defun trim-list (list &optional (limit 100))
  (handler-case
      (if (< limit (length list))
          (nconc (sera:nsubseq list 0 (1- limit)) (list "…"))
          list)
    (error ()
      ;; Improper list.
      list)))
