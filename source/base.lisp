;;; base.lisp --- main entry point into Next

;; TODO: Rename to start.lisp.  This should probably be the last file before the
;; modes in the .asd.

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
    (:name :init-file
           :short #\i
           :long "init-file"
           :arg-parser #'identity
           :description "Set path to initialization file.
Set to '-' to read standard input instead.")
    (:name :no-init
           :short #\Q
           :long "no-init"
           :description "Do not load the user init file."))

  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

;; TODO: Find a way to list/introspect available platform port methods from a
;; running Next.

(define-command quit ()
  "Quit Next."
  (hooks:run-hook (hooks:object-hook *interface* 'before-exit-hook))
  (kill-interface *interface*)
  (kill-port (port *interface*)))

(define-command quit-after-clearing-session ()
  "Clear session then quit Next."
  (setf
   (session-store-function *interface*) nil
   (session-restore-function *interface*) nil)
  (uiop:delete-file-if-exists (session-path *interface*))
  (kill-interface *interface*)
  (kill-port (port *interface*)))

(define-deprecated-command kill ()
  "Deprecated by `quit'."
  (quit))

(defun set-debug-level (level)
  "Supported values for LEVEL are
- `:debug': Debug logging.
- `t': Normal logging."
  (match level
    (:debug
      (log:config :debug)
      (setf (uiop:getenv "G_MESSAGES_DEBUG") "all"))
    (_
     (log:config :info)
     (setf (uiop:getenv "G_MESSAGES_DEBUG") ""))))

@export
(defun entry-point ()
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
    (setf *options* options
          *free-args* free-args)
    (start :urls free-args
           :non-interactive t))
  (handler-case (progn (run-loop (port *interface*))
                       (kill-interface *interface*))
    ;; Catch a C-c, don't print a full stacktrace.
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (kill-interface *interface*)
          (kill-port (port *interface*))
          (format t "Bye!~&")
          (uiop:quit)))))

(defun ping-platform-port (&optional (bus-type (session-server-addresses)))
  (dbus:with-open-bus (bus bus-type)
    (member-string +platform-port-name+ (dbus:list-names bus))))

(defun initialize-port ()
  "Start platform port if necessary.
Error out if no platform port can be started."
  ;; TODO: With D-Bus we can "watch" a connection.  Is this implemented in the
  ;; CL library?  Else we could bind initialize-port to a D-Bus notification.
  (let ((port-running (ping-platform-port)))
    (unless (or port-running
                (and (port *interface*)
                     (running-process (port *interface*))))
      (run-program (port *interface*)))
    (let ((max-attempts (/ (platform-port-poll-duration *interface*)
                           (platform-port-poll-interval *interface*))))
      ;; Poll the platform port in case it takes some time to start up.
      (loop while (not port-running)
            repeat max-attempts
            do (unless (setf port-running (ping-platform-port))
                 (sleep (platform-port-poll-interval *interface*))))
      (unless port-running
        (progn
          (kill-port (port *interface*))
          (kill-interface *interface*))
        (error "Could not connect to platform port: ~a" (path (port *interface*)))))))

(defun init-file-path (&optional (filename "init.lisp"))
  ;; This can't be a regular variable or else the value will be hard-coded at
  ;; compile time.  It seems to be hard-coded with (eval-when (:execute) ...) as well.
  "The path where the system will look to load an init file from."
  (or (getf *options* :init-file)
      (xdg-config-home filename)))

(defun load-lisp-file (file &key interactive)
  "Load the provided lisp file.
If FILE is \"-\", read from the standard input.
If INTERACTIVE is non-nil, allow the debugger on errors."
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
        (let ((message "Error: could not load the init file"))
          ;; (echo-warning message)
          (if interactive
              (error (format nil "~a:~&~a" message c))
              (progn
                (format *error-output* "~%~a~&~a~&" (cl-ansi-text:red message) c)
                (uiop:quit 1))))))))

(define-command load-file (&key interactive)
  "Load the prompted Lisp file.
If INTERACTIVE is non-nil, allow the debugger on errors."
  (with-result (file-name-input (read-from-minibuffer
                                 (make-instance 'minibuffer
                                                :input-prompt "Load file:")))
    (load-lisp-file file-name-input :interactive interactive)))

(define-command load-init-file (&key (init-file (init-file-path))
                                     interactive)
  "Load or reload the init file.
If INTERACTIVE is non-nil, allow the debugger on errors."
  (load-lisp-file init-file :interactive interactive))

(defun default-startup (&optional urls)
  "Make a window and load URLS in new buffers.
This function is suitable as a `remote-interface' `startup-function'."
  (if urls
      (open-urls urls)
      ;; TODO: Test if network is available.  If not, display help,
      ;; otherwise display start-page-url.
      (let ((window (rpc-window-make))
            (buffer (help)))
        (window-set-active-buffer window buffer)))
  (match (session-restore-function *interface*)
    ((guard f f)
     (funcall f))))

@export
(defun start (&key urls
                (init-file (init-file-path))
                non-interactive)
  "Start Next and load URLS if any.
A new `*interface*' is instantiated.
The platform port is automatically started if needed.
Finally, run the `*after-init-hook*'."
  (let ((startup-timestamp (local-time:now)))
    (format t "Next version ~a~&" +version+)
    ;; Randomness should be seeded as early as possible to avoid generating
    ;; deterministic tokens.
    (setf *random-state* (make-random-state t))
    ;; Reset `*after-init-hook*' or else the handlers will stack.
    (setf *after-init-hook* nil)
    (unless (getf *options* :no-init)
      (load-lisp-file init-file :interactive (not non-interactive)))
    ;; create the interface object
    (when *interface*
      (kill-interface *interface*)
      ;; It's important to set it to nil or else if we re-run this function,
      ;; (make-instance 'remote-interface) will be run while an existing
      ;; *interface* is still floating around.
      (setf *interface* nil))
    (setf *interface* (make-instance 'remote-interface
                                     :non-interactive non-interactive
                                     :startup-timestamp startup-timestamp))
    ;; Start the port after the interface so that we don't overwrite the log when
    ;; an instance is already running.
    (handler-case
        (initialize-port)
      (error (c)
        (log:error "~a~&~a" c
                   "Make sure the platform port executable is either in the
PATH or set in you ~/.config/next/init.lisp, for instance:

     (setf +platform-port-command+
         \"~/common-lisp/next/ports/gtk-webkit/next-gtk-webkit\")")
        (when non-interactive
          (uiop:quit))))
    (setf (slot-value *interface* 'init-time)
          (local-time:timestamp-difference (local-time:now) startup-timestamp))
    (handler-case
        (hooks:run-hook '*after-init-hook*)
      (error (c)
        (log:error "In *after-init-hook*: ~a" c)))
    (handler-case
        (funcall (startup-function *interface*) (or urls *free-args*))
      (error (c)
        (log:error "In startup-function ~a: ~a" (startup-function *interface*) c)))))

(define-command next-init-time ()
  "Return the duration of Next initialization."
  (echo "~,2f seconds" (slot-value *interface* 'init-time)))
