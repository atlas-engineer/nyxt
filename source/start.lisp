;;; start.lisp --- main entry point into Next

(in-package :next)

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
    (:name :no-session
           :short #\S
           :long "no-session"
     :description "Do not load any session.")
    (:name :session
           :short #\s
           :long "session"
           :arg-parser #'identity
     :description (format nil "The argument can be empty (same effect as --no-session), a file if it contains a
slash (prefix with ./ to refer to files in current directory) or the basename of
a file in the sessions directory '~a'.
Warning: any existing file will be overwritten."
                          (xdg-data-home "sessions"))))
  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

(define-command quit ()
  "Quit Next."
  (hooks:run-hook (before-exit-hook *browser*))
  (loop for window in (window-list)
        do (ffi-window-delete window))
  (ffi-kill-browser *browser*)
  (when (socket-thread *browser*)
    (ignore-errors
     (bt:destroy-thread (socket-thread *browser*))))
  (when (uiop:file-exists-p (socket-path *browser*))
    (log:info "Deleting socket ~a" (socket-path *browser*))
    (uiop:delete-file-if-exists (socket-path *browser*)))
  (unless *keep-alive*
    (uiop:quit 0 nil)))

(define-command quit-after-clearing-session ()
  "Clear session then quit Next."
  (setf
   (session-store-function *browser*) nil
   (session-restore-function *browser*) nil)
  (uiop:delete-file-if-exists *session*)
  (quit))

(defun set-debug-level (level)
  "Supported values for LEVEL are
   - `:debug': Debug logging.
   - `t': Normal logging."
  (match level
    (:debug
     (log:config :debug))
    (otherwise
     (log:config :info))))

(defun derive-session (name)
  "Derive session file from NAME.
If NAME has a slash, use the file it refers to.
Without slash, NAME (with .lisp appended if not already there), store in
the (xdg-data-home \"sessions \") folder."
  (cond
    ((uiop:emptyp name)
     "")
    ((search "/" name)
     name)
    (t
     (let ((name (format nil "~a/~a"
                         (xdg-data-home "sessions")
                         name)))
       (unless (str:ends-with? ".lisp" name :ignore-case t)
         (setf name (str:concat name ".lisp")))
       name))))

(serapeum:export-always 'entry-point)
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
    (setf *session*
          (if (getf options :no-session)
              ""
              (or (getf options :session)
                  *session*)))
    (setf *options* options
          *free-args* free-args)
    (setf *keep-alive* nil)             ; Not a REPL.
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
            (t (error "~a:~&~a" message c))))))))

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
   suitable as a `browser' `startup-function'."
  (let ((window (window-make *browser*))
        (buffer (help)))
    (if urls
        (open-urls urls)
        (window-set-active-buffer window buffer)))
  (match (session-restore-function *browser*)
    ((guard f f)
     (when (uiop:file-exists-p *session*)
       (log:info "Restoring session '~a'" *session*)
       (funcall f)))))

(defun listen-socket ()
  (ensure-parent-exists (socket-path *browser*))
  ;; TODO: Catch error against race conditions?
  (iolib:with-open-socket (s :address-family :local
                             :connect :passive
                             :local-filename (socket-path *browser*))
    (loop as connection = (iolib:accept-connection s)
          while connection
          do (progn (match (str:split (string #\newline)
                                      (alex:read-stream-content-into-string connection)
                                      :omit-nulls t)
                      ((guard urls urls)
                       (log:info "External process requested URL(s): ~{~a~^, ~}" urls)
                       (ffi-within-renderer-thread
                        *browser*
                        (lambda () (open-urls urls))))
                      (_
                       (log:info "External process pinged Next.")))
                    ;; If we get pinged too early, we not have a current-window yet.
                    (when (current-window)
                     (ffi-window-to-foreground (current-window))))))
  (log:info "Listening on socket ~s" (socket-path *browser*)))

(defun bind-socket-or-quit (urls)
  "If another Next is listening on the socket, tell it to open URLS.
Otherwise bind socket."
  (if (ignore-errors
       (iolib:with-open-socket (s :address-family :local
                                  :remote-filename (socket-path *browser*))
         (iolib:socket-connected-p s)))
      (progn
        (if urls
            (log:info "Next already started, requesting to open URL(s): ~{~a~^, ~}" urls)
            (log:info "Next already started." urls))
        (iolib:with-open-socket (s :address-family :local
                                   :remote-filename (socket-path *browser*))
          (format s "~{~a~%~}" urls))
        (uiop:quit))
      (progn
        (uiop:delete-file-if-exists (socket-path *browser*))
        (setf (socket-thread *browser*) (bt:make-thread #'listen-socket)))))

(serapeum:export-always 'start)
(defun start (&key urls (init-file (init-file-path)))
  "Start Next and load URLS if any. A new `*browser*' is
   instantiated. The platform port is automatically started if
   needed. Finally, run the `*after-init-hook*'."
  (let ((startup-timestamp (local-time:now)))
    (format t "Next version ~a~&" +version+)
    (setf *session* (derive-session *session*))
    (unless (getf *options* :no-init)
      (load-lisp-file init-file :interactive t))
    (setf *browser* (make-instance *browser-class*
                                   :startup-timestamp startup-timestamp))
    (when (single-instance-p *browser*)
      (bind-socket-or-quit urls))
    (ffi-initialize *browser* urls startup-timestamp)))

(define-command next-init-time ()
  "Return the duration of Next initialization."
  (echo "~,2f seconds" (slot-value *browser* 'init-time)))
