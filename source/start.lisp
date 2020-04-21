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
    (:name :init
           :short #\i
           :long "init"
           :arg-parser #'identity
           :description "Set path to initialization file.
Set to '-' to read standard input instead.")
    (:name :no-init
           :short #\I
           :long "no-init"
           :description "Do not load the user init file.")
    (:name :eval
           :short #\e
           :long "eval"
           :arg-parser #'identity
           :description "Eval the Lisp expressions.  Can be specified multiple times.")
    (:name :load
           :short #\l
           :long "load"
           :arg-parser #'identity
           :description "Load the Lisp file.  Can be specified multiple times.")
    (:name :script
           :long "script"
           :arg-parser #'identity
           :description "Load the Lisp file (skip #! line if any), skip init file, then quit.")
    (:name :remote
           :short #\r
           :long "remote"
           :description "Send the --eval and --load arguments to the running instance of Next.")
    (:name :session
           :short #\s
           :long "session"
           :arg-parser #'identity
           :description (format nil "The argument can be empty (same effect as --no-session),
a file if it contains a slash (prefix with ./ to refer
to files in current directory) or the basename of a file
in the sessions directory '~a'.
Warning: any existing file will be overwritten."
                          (xdg-data-home "sessions")))
    (:name :no-session
           :short #\S
           :long "no-session"
           :description "Do not load any session."))
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

(defun derive-session (name) ; TODO: Remove *session* and use same technique as `init-file-path'?
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

;; From sbcl/src/code/load.lisp
(defun maybe-skip-shebang-line (stream)
  (let ((p (file-position stream)))
    (when p
      (flet ((next () (read-byte stream nil)))
        (unwind-protect
             (when (and (eq (next) (char-code #\#))
                        (eq (next) (char-code #\!)))
               (setf p nil)
               (loop for x = (next)
                     until (or (not x) (eq x (char-code #\newline)))))
          (when p
            (file-position stream p)))))))

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

    (when (getf options :script)
      (with-open-file (f (getf options :script) :element-type :default)
        (maybe-skip-shebang-line f)
        (load-lisp f))
      (uiop:quit))

    (when (or (getf options :load)
              (getf options :eval))
      (unless (getf options :no-init)
        (load-lisp (init-file-path) :interactive nil))
      ;; We need a browser instance so that listen-socket-p can know the socket path.
      ;; TODO: Use (get-default ...) instead?  Ideally, socket could also
      ;; be a command line parameter so that multiple instances can be started.
      (setf *browser* (make-instance *browser-class*))
      (loop for (opt value . _) on options
            do (match opt
                 (:load (let ((value (uiop:truename* value)))
                          ;; Absolute path is necessary since remote process may have
                          ;; a different working directory.
                          (if (getf options :remote)
                              (remote-eval (format nil "~s" `(load-lisp ,value)))
                              (load-lisp value))))
                 (:eval (if (getf options :remote)
                            (remote-eval value)
                            (eval-expr value)))))
      (uiop:quit))

    (setf *session*
          (if (getf options :no-session)
              ""
              (or (getf options :session)
                  *session*)))
    (setf *options* options)
    (setf *keep-alive* nil)             ; Not a REPL.
    (start :urls free-args)))

(defun init-file-path (&optional (filename "init.lisp"))
  "The path where the system will look to load an init file from. This
   can't be a regular variable or else the value will be hard-coded at
   compile time.  It seems to be hard-coded with (eval-when (:execute)
   ...) as well."
  (or (getf *options* :init)
      (xdg-config-home filename)))

(defparameter *load-init-error-message* "Error: Could not load the init file")
(defparameter *load-init-type-error-message* (str:concat *load-init-error-message*
                                                         " because of a type error"))

(declaim (ftype (function (trivial-types:pathname-designator &key (:interactive t) (:package (or null package))))
                load-lisp))
(defun load-lisp (file &key interactive package)
  "Load the Lisp FILE (or stream).
   If FILE is \"-\", read from the standard input.
   If INTERACTIVE is t, allow the debugger on errors. If :running, show
   an error but don't quit the Lisp process. If nil, quit Lisp (especially
   useful when Next starts up)."
  (let ((*package* (or (find-package package) *package*)))
    (handler-case
        (progn
          (when (equal "" file)
            (error "Can't load empty file name."))
          (cond
            ((and (not (streamp file)) (string= (pathname-name file) "-"))
             (progn
               (format t "Loading Lisp from standard input...")
               (loop for object = (read *standard-input* nil :eof)
                     until (eq object :eof)
                     do (eval object))))
            ((streamp file)
             (load file))
            ((uiop:file-exists-p file)
             (format t "~&Loading Lisp file ~s...~&" file)
             (load file))))
      (error (c)
        ;; TODO: Handle warning from `echo'.
        (let ((message (if (subtypep (type-of c) 'type-error)
                           *load-init-type-error-message*
                           *load-init-error-message*)))
          (cond
            ((equal interactive :running)
             (echo-safe (format nil "~a: ~a" message c))
             (notify (str:concat message ".")))
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
    (load-lisp file-name-input :interactive interactive)))

(define-command load-init-file (&key (init-file (init-file-path))
                                (interactive :running))
  "Load or reload the init file.
   If INTERACTIVE is t, allow the debugger on errors.
   If :running, show an error but don't quit the Lisp process."
  (load-lisp init-file :interactive interactive))

(defun eval-expr (expr)
  "Evaluate the form EXPR (string) and print the result of the last expresion."
  (handler-case
      (with-input-from-string (input expr)
        (format t "~a~&"
                (loop with result = nil
                      for object = (read input nil :eof)
                      until (eq object :eof)
                      do (setf result (eval object))
                      finally (return result))))
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
  (when (startup-error-reporter-function *browser*)
    (funcall-safely (startup-error-reporter-function *browser*)))
  (match (session-restore-function *browser*)
    ((guard f f)
     (when (uiop:file-exists-p *session*)
       (log:info "Restoring session '~a'" *session*)
       (funcall f)))))

(defun open-external-urls (urls)
  (if urls
      (log:info "Externally requested URL(s): ~{~a~^, ~}" urls)
      (log:info "Externally pinged."))
  (ffi-within-renderer-thread
   *browser*
   (lambda () (open-urls urls))))

(defun listen-socket ()
  (ensure-parent-exists (socket-path *browser*))
  ;; TODO: Catch error against race conditions?
  (iolib:with-open-socket (s :address-family :local
                             :connect :passive
                             :local-filename (socket-path *browser*))
    (loop as connection = (iolib:accept-connection s)
          while connection
          do (progn (match (alex:read-stream-content-into-string connection)
                      ((guard expr (not (uiop:emptyp expr)))
                       (log:info "External evaluation request: ~s" expr)
                       (eval-expr expr))
                      (_
                       (log:info "External process pinged Next.")))
                    ;; If we get pinged too early, we do not have a current-window yet.
                    (when (current-window)
                     (ffi-window-to-foreground (current-window))))))
  (log:info "Listening on socket ~s" (socket-path *browser*)))

(defun listening-socket-p ()
  (ignore-errors
   (iolib:with-open-socket (s :address-family :local
                              :remote-filename (socket-path *browser*))
     (iolib:socket-connected-p s))))

(defun bind-socket-or-quit (urls)
  "If another Next is listening on the socket, tell it to open URLS.
Otherwise bind socket."
  (if (listening-socket-p)
      (progn
        (if urls
            (log:info "Next already started, requesting to open URL(s): ~{~a~^, ~}" urls)
            (log:info "Next already started." urls))
        (iolib:with-open-socket (s :address-family :local
                                   :remote-filename (socket-path *browser*))
          (format s "~s" `(open-external-urls ',urls)))
        (uiop:quit))
      (progn
        (uiop:delete-file-if-exists (socket-path *browser*))
        (setf (socket-thread *browser*) (bt:make-thread #'listen-socket)))))

(defun remote-eval (expr)
  "If another Next is listening on the socket, tell it to evaluate EXPR."
  (if (listening-socket-p)
      (progn
        (iolib:with-open-socket (s :address-family :local
                                   :remote-filename (socket-path *browser*))
          (write-string expr s))
        (uiop:quit))
      (progn
        (log:info "No instance running.")
        (uiop:quit))))

(serapeum:export-always 'start)
(defun start (&key urls (init-file (init-file-path)))
  "Start Next and load URLS if any. A new `*browser*' is
   instantiated. The platform port is automatically started if
   needed. Finally, run the `*after-init-hook*'."
  (let ((startup-timestamp (local-time:now)))
    (format t "Next version ~a~&" +version+)
    (setf *session* (derive-session *session*))
    (unless (getf *options* :no-init)
      (handler-case
          (progn
            (load-lisp init-file :interactive t)
            (setf *browser* (make-instance *browser-class*
                                           :startup-timestamp startup-timestamp)))
        (error (c)
          (setf *browser* (make-instance *browser-class*
                                         :startup-error-reporter-function
                                         (lambda () (error-in-new-window
                                                     "*Init file errors*"
                                                     (format nil "~a" c)))
                                         :startup-timestamp startup-timestamp)))))
    (when (single-instance-p *browser*)
      (bind-socket-or-quit urls))
    (ffi-initialize *browser* urls startup-timestamp)))

(define-command next-init-time ()
  "Return the duration of Next initialization."
  (echo "~,2f seconds" (slot-value *browser* 'init-time)))
