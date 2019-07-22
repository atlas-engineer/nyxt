;;; base.lisp --- main entry point into Next

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
    (:name :init-file
           :short #\i
           :long "init-file"
           :arg-parser #'identity
           :description "Set path to initialization file.
Set to '-' to read standard input instead."))

  (handler-bind ((opts:unknown-option #'handle-malformed-cli-arg)
                 (opts:missing-arg #'handle-malformed-cli-arg)
                 (opts:arg-parser-failed #'handle-malformed-cli-arg))
    (opts:get-opts)))

;; TODO: Find a way to list/introspect available platform port methods from a
;; running Next.

(define-command kill ()
  "Quit Next."
  (kill-interface *interface*)
  (kill-program (port *interface*)))

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
     (setf (uiop:getenv "G_MESSAGES_DEBUG") nil))))

(defun entry-point ()
  (multiple-value-bind (options free-args)
      (parse-cli-args)
    (when (getf options :help)
      (opts:describe :prefix "Next command line usage:")
      (uiop:quit))
    (when (getf options :version)
      (format t "Next ~a~&" +version+)
      (uiop:quit))
    (when (getf options :verbose)
      (set-debug-level :debug)
      (format t "Arguments parsed: ~a and ~a~&" options free-args))
    (setf *options* options
          *free-args* free-args)
    (apply #'start free-args))
  (handler-case (progn (run-loop (port *interface*))
                       (kill-interface *interface*))
    ;; Catch a C-c, don't print a full stacktrace.
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (kill-interface *interface*)
          (kill-program (port *interface*))
          (format t "Bye!~&")
          (uiop:quit)))))

(defun ping-platform-port (&optional (bus-type (dbus:session-server-addresses)))
  (dbus:with-open-bus (bus bus-type)
    (member-string +platform-port-name+ (dbus:list-names bus))))

(defmethod initialize-port ((interface remote-interface) &optional (urls *free-args*))
  "Start platform port if necessary and make a first window."
  ;; TODO: With D-Bus we can "watch" a connection.  Is this implemented in the
  ;; CL library?  Else we could bind initialize-port to a D-Bus notification.
  (let ((port-running (ping-platform-port)))
    (unless (or port-running
                (and (port interface)
                     (running-process (port interface))))
      (handler-case
          (run-program (port interface))
        (error (c)
          (log:error "~a~&~a" c
                     "Make sure the platform port executable is either in the
PATH or set in you ~/.config/next/init.lisp, for instance:

(setf (get-default 'port 'path)
      \"~/common-lisp/next/ports/gtk-webkit/next-gtk-webkit\")")
          (uiop:quit))))
    (let ((max-attempts (/ (platform-port-poll-duration interface)
                          (platform-port-poll-interval interface))))
      ;; Poll the platform port in case it takes some time to start up.
      (loop while (not port-running)
            repeat max-attempts
            do (unless (setf port-running (ping-platform-port))
                 (sleep (platform-port-poll-interval interface))))
      (if port-running
          ;; TODO: MAKE-WINDOW should probably take INTERFACE as argument.
          (let ((buffer (nth-value 1 (make-window))))
            (set-url-buffer (if urls
                                (first urls)
                                (start-page-url interface))
                            buffer)
            ;; We can have many URLs as positional arguments.
            (loop for url in (rest urls) do
              (let ((buffer (make-buffer)))
                (set-url-buffer url buffer))))
          (progn
            (log:error "Could not connect to platform port: ~a" (path (port interface)))
            (kill-program (port interface))
            (kill-interface interface)
            (uiop:quit))))))

(defun init-file-path (&optional (file "init.lisp"))
  ;; This can't be a regular variable or else the value will be hard-coded at
  ;; compile time.  It seems to be hard-coded with (eval-when (:execute) ...) as well.
  "The path where the system will look to load an init file from."
  (xdg-config-home (or (getf *options* :init-file) file)))

(defun load-lisp-file (file)
  "Load the provided lisp file.
Interactively, prompt for FILE.
If FILE is \"-\", read from the standard input."
  (handler-case (if (string= (pathname-name file) "-")
                    (progn
                      (log:info "Loading configuration from standard input...")
                      (loop for object = (read *standard-input* nil :eof)
                            until (eq object :eof)
                            do (eval object)))
                    (progn
                      (log:info "Loading configuration from ~a..." file)
                      (load file :if-does-not-exist nil)))
    (error (c)
      ;; TODO: Handle warning from `echo'.
      (log:warn "Error: we could not load the Lisp file ~a: ~a" file c)
      (echo "Error: we could not load the Lisp file ~a: ~a" file c))))

(define-command load-file ()
  "Load the provided lisp file.
Interactively, prompt for FILE.
If FILE is \"-\", read from the standard input."
  (with-result (file-name-input (read-from-minibuffer
                                 (minibuffer *interface*)
                                 :input-prompt "Load file:"))
    (load-lisp-file file-name-input)))

(define-command load-init-file (root-mode &optional (init-file (init-file-path)))
  "Load or reload the init file."
  (load-lisp-file init-file))

(defun start (&rest urls)
  (log:info +version+)
  ;; Randomness should be seeded as early as possible to avoid generating
  ;; deterministic tokens.
  (setf *random-state* (make-random-state t))
  (load-lisp-file (init-file-path))
  ;; create the interface object
  (unless (eq swank:*communication-style* :fd-handler)
    (log:warn "swank:*communication-style* is set to ~s, recommended value is :fd-handler"
              swank:*communication-style*))
  (when *interface*
    (kill-interface *interface*)
    ;; It's important to set it to nil or else if we re-run this function,
    ;; (make-instance 'remote-interface) will be run while an existing
    ;; *interface* is still floating around.
    (setf *interface* nil))
  (setf *interface* (make-instance 'remote-interface))
  ;; Start the port after the interface so that we don't overwrite the log when
  ;; an instance is already running.
  (initialize-port *interface* (or urls *free-args*)))

(define-key "C-x C-c" 'kill)
(define-key "C-[" 'switch-buffer-previous)
(define-key "C-]" 'switch-buffer-next)
(define-key "C-x b" 'switch-buffer)
(define-key "C-x k" 'delete-buffer)
(define-key "C-x Left" 'switch-buffer-previous)
(define-key "C-x Right" 'switch-buffer-next)
(define-key "C-l" 'set-url-current-buffer)
(define-key "M-l" 'set-url-new-buffer)
(define-key "C-m k" 'bookmark-delete)
(define-key "C-t" 'make-visible-new-buffer)
(define-key "C-m u" 'bookmark-url)
(define-key "C-x w" 'delete-current-buffer)
;; TODO: Rename to inspect-variable?  Wouldn't describe-variable be more familiar?
(define-key "C-h v" 'variable-inspect)
(define-key "C-h c" 'command-inspect)
(define-key "C-o" 'load-file)
(define-key "C-h s" 'start-swank)
(define-key "M-x" 'execute-command)
(define-key "C-x 5 2" 'new-window)
(define-key "C-x 5 0" 'delete-window)
(define-key "C-x q" (lambda () (echo-dismiss (minibuffer *interface*))))

(define-key :scheme :vi-normal
  "Z Z" 'kill
  "[" 'switch-buffer-previous
  "]" 'switch-buffer-next
  "g b" 'switch-buffer
  "d" 'delete-buffer
  "D" 'delete-current-buffer
  "B" 'make-visible-new-buffer
  "o" 'set-url-current-buffer
  "O" 'set-url-new-buffer
  "m u" 'bookmark-url
  "m d" 'bookmark-delete
  "C-o" 'load-file
  "C-h v" 'variable-inspect
  "C-h c" 'command-inspect
  "C-h s" 'start-swank
  ":" 'execute-command
  "W" 'new-window)
