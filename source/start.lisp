;;; start.lisp --- main entry point into Next

(in-package :next)
(annot:enable-annot-syntax)

(defun handle-malformed-cli-arg (condition)
  (format t "Error parsing argument ~a: ~a.~&" (opts:option condition) condition)
  (opts:describe)
  (uiop:quit))

(defun parse-cli-args ()
  "Parse command line arguments."
  (opts:define-opts
    (:name :help
           :description "Print this help and exit."
           :short #\h
           :long "help")
    (:name :verbose
           :short #\v
           :long "verbose"
           :description "Print debugging information to stdout.")
    (:name :version
           :long "version"
           :description "Print version and exit.")
    (:name :eval
           :long "eval"
           :arg-parser #'identity
           :description "Eval the Lisp expression.")
    (:name :init-file
           :short #\i
           :long "init-file"
           :arg-parser #'identity
           :description "Set path to initialization file.
Set to '-' to read standard input instead.")
    (:name :no-init
           :short #\Q
           :long "no-init"
           :description "Do not load the user init file.")
    (:name :session
           :short #\s
           :long "session"
           :arg-parser #'identity
           :description "With --session nil, don't restore or store the session."))
  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

(define-command quit ()
  "Quit Next."
  (next-hooks:run-hook (before-exit-hook *interface*))
  (kill-interface *interface*))

(define-command quit-after-clearing-session ()
  "Clear session then quit Next."
  (setf
   (session-store-function *interface*) nil
   (session-restore-function *interface*) nil)
  (uiop:delete-file-if-exists (session-path *interface*))
  (kill-interface *interface*))

(defun set-debug-level (level)
  "Supported values for LEVEL are
   - `:debug': Debug logging.
   - `t': Normal logging."
  (match level
    (:debug
     (log:config :debug))
    (otherwise
     (log:config :info))))

@export
(defun entry-point ()
  "Read the CLI arguments and start the interface."
  (multiple-value-bind (options free-args)
      (parse-cli-args)
    (when (getf options :help)
      (opts:describe :prefix "Next command line usage:

next [options] [urls]")
      (uiop:quit))
    (when (getf options :version)
      (format t "Next ~a~&" +version+)
      (uiop:quit))
    (when (getf options :verbose)
      (set-debug-level :debug)
      (format t "Arguments parsed: ~a and ~a~&" options free-args))
    (when (getf options :eval)
      (unless (getf options :no-init)
        (load-lisp-file (init-file-path) :interactive nil))
      (eval-expr (getf options :eval))
      (uiop:quit))
    (when (getf options :session)
      (when (string-equal (getf options :session) "nil")
        (setf next:*use-session* nil)))
    (setf *options* options
          *free-args* free-args)
    (start :urls free-args)))

(defun init-file-path (&optional (filename "init.lisp"))
  "The path where the system will look to load an init file from. This
   can't be a regular variable or else the value will be hard-coded at
   compile time.  It seems to be hard-coded with (eval-when (:execute)
   ...) as well."
  (or (getf *options* :init-file)
      (xdg-config-home filename)))

(defparameter *load-init-error-message* "Error: we could not load the init file")
(defparameter *load-init-type-error-message* (str:concat *load-init-error-message* " because of a type error."))

(defun load-lisp-file (file &key interactive)
  "Load the provided lisp file.
   If FILE is \"-\", read from the standard input.
   If INTERACTIVE is t, allow the debugger on errors. If :running, show
   an error but don't quit the Lisp process. If nil, quit Lisp (especially
   useful when Next starts up)."
  (unless (str:emptyp (namestring file))
    (handler-case (if (string= (pathname-name file) "-")
                      (progn
                        (format t "Loading configuration from standard input...")
                        (loop for object = (read *standard-input* nil :eof)
                              until (eq object :eof)
                              do (eval object)))
                      (when (uiop:file-exists-p file)
                        (format t "~&Loading configuration from ~s...~&" file)
                        (load file)))
      (error (c)
        ;; TODO: Handle warning from `echo'.
        (let ((message (if (subtypep (type-of c) 'type-error)
                           *load-init-type-error-message*
                           *load-init-error-message*)))
          (cond
            ((equal interactive :running)
             (echo-safe "Could not load the init file: ~a" c)
             (notify "We could not load the init file."))
            ((null interactive)
             (format *error-output* "~%~a~&~a~&" (cl-ansi-text:red message) c)
             (uiop:quit 1))
            (t (error (format nil "~a:~&~s" message c)))))))))

(define-command load-file (&key (interactive :running))
  "Load the prompted Lisp file.
   If INTERACTIVE is t, allow the debugger on errors.
   If :running, show an error but don't quit the Lisp process."
  (with-result (file-name-input (read-from-minibuffer
                                 (make-minibuffer
                                  :input-prompt "Load file"
                                  :show-completion-count nil)))
    (load-lisp-file file-name-input :interactive interactive)))

(define-command load-init-file (&key (init-file (init-file-path))
                                (interactive :running))
  "Load or reload the init file.
   If INTERACTIVE is t, allow the debugger on errors.
   If :running, show an error but don't quit the Lisp process."
  (load-lisp-file init-file :interactive interactive))

(defun eval-expr (expr)
  "Evaluate the form EXPR (string) and print its result.
EXPR must contain one single Lisp form. Use `progn' if needed."
  (handler-case
      (print (eval (read-from-string expr)))
    (error (c)
      (format *error-output* "~%~a~&~a~&" (cl-ansi-text:red "Evaluation error:") c)
      (uiop:quit 1))))

(defun default-startup (&optional urls)
  "Make a window and load URLS in new buffers. This function is
   suitable as a `interface' `startup-function'."
  (when urls
    (open-urls urls))
  (let ((window (ipc-window-make *interface*))
        (buffer (help)))
    (window-set-active-buffer window buffer))
  (match (session-restore-function *interface*)
    ((guard f f)
     (when *use-session*
       (funcall f)))))

@export
(defun start (&key urls (init-file (init-file-path)))
  "Start Next and load URLS if any. A new `*interface*' is
   instantiated. The platform port is automatically started if
   needed. Finally, run the `*after-init-hook*'."
  (let ((startup-timestamp (local-time:now)))
    (format t "Next version ~a~&" +version+)
    (unless (getf *options* :no-init)
      (load-lisp-file init-file :interactive t))
    ;; TODO: change gtk-interface to interface
    (setf *interface* (make-instance 'gtk-interface :startup-timestamp startup-timestamp))
    (setf (slot-value *interface* 'init-time)
          (local-time:timestamp-difference (local-time:now) startup-timestamp))
    (handler-case
        (next-hooks:run-hook *after-init-hook*)
      (error (c)
        (log:error "In *after-init-hook*: ~a" c)))
    (handler-case
        (funcall (startup-function *interface*) (or urls *free-args*))
      (error (c)
        (log:error "In startup-function ~a: ~a" (startup-function *interface*) c)))
    (log4cl-impl:add-appender log4cl:*root-logger* (make-instance 'messages-appender)))
  (initialize *interface*))

(define-command next-init-time ()
  "Return the duration of Next initialization."
  (echo "~,2f seconds" (slot-value *interface* 'init-time)))
