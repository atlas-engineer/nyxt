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

;; TODO: This is rather low level and should probably not be a command.
;; We can safely remove it once we can EVAL within Next.
(define-command server-methods-inspect ()
  "List the XML-RPC methods supported by the platform port."
  (echo (minibuffer *interface*) (cl-strings:join
                      (sort (list-methods *interface*) #'string<)
                      :separator "
")))

(define-command kill ()
  "Quit Next."
  (kill-interface *interface*)
  (kill-program (port *interface*)))

(defun start-with-port ()
  (multiple-value-bind (options free-args)
      (parse-cli-args)
    (when (getf options :help)
      (opts:describe :prefix "Next command line usage:")
      (uiop:quit))
    (when (getf options :verbose)
      (log:config :debug)
      (setf (uiop:getenv "G_MESSAGES_DEBUG") "all")
      (format t "Arguments parsed: ~a and ~a~&" options free-args))
    (setf *options* options
          *free-args* free-args))
  (handler-case (start :with-platform-port-p t)
    ;; Catch a C-c, don't print a full stacktrace.
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format t "Bye!~&")
           (uiop:quit)))))

(defmethod initialize-port ((interface remote-interface))
  (let* ((port-running nil)
         (max-seconds-to-wait 5.0)
         (max-attemps (/ max-seconds-to-wait (platform-port-poll-interval interface))))
    (loop while (not port-running)
          repeat max-attemps do
      (handler-case
          (progn
            (when (list-methods interface)
              (setf port-running t)))
        (error (c)
          (log:debug "Could not communicate with port: ~a" c)
          (log:info "Polling platform port '~a'...~%" (platform-port-socket interface))
          (sleep (platform-port-poll-interval interface))
          (setf port-running nil))))
    (when port-running
          ;; TODO: MAKE-WINDOW should probably take INTERFACE as argument.
      (let ((buffer (nth-value 1 (make-window))))
        (set-url-buffer (if *free-args* (car *free-args*) (start-page-url interface)) buffer)
        ;; We can have many URLs as positional arguments.
        (loop for url in (cdr *free-args*) do
          (let ((buffer (make-buffer)))
            (set-url-buffer url buffer)))))))

(defvar *init-file-path* (xdg-config-home "init.lisp")
  "The path where the system will look to load an init file from.")

;; TODO: Turn into a command?
(defun load-init-file (&optional (init-file *init-file-path*))
  "Load the user configuration if it exists."
  (handler-case (if (string= (pathname-name init-file) "-")
                    (progn
                      (log:info "Initializing from standard input...")
                      (loop for object = (read *standard-input* nil :eof)
                            until (eq object :eof)
                            do (eval object)))
                    (load init-file :if-does-not-exist nil))
    (error (c)
      (log:warn "Error: we could not load your init file: ~a" c)
      (when *interface*
        (echo (minibuffer *interface*)
              (format nil "Error: we could not load your init file: ~a" c))))))

(defun start (&key (with-platform-port-p nil))
  (when (getf *options* :init-file)
    (setf *init-file-path* (getf *options* :init-file)))
  (load-init-file)
  ;; create the interface object
  (unless (eq swank:*communication-style* :fd-handler)
    (log:warn "swank:*communication-style* is set to ~s, recommended value is :fd-handler"
              swank:*communication-style*))
  (unless *interface*
    (setf *interface* (make-instance 'remote-interface)))
  ;; Start the port after the interface so that we don't overwrite the log when
  ;; an instance is already running.
  (when with-platform-port-p
    (run-program (port *interface*)))
  (initialize-port *interface*)
  (when with-platform-port-p
    (run-loop (port *interface*))
    (kill-interface *interface*))
  t)

(define-key *global-map* (key "C-x C-c") 'kill)
(define-key *global-map* (key "C-[") 'switch-buffer-previous)
(define-key *global-map* (key "C-]") 'switch-buffer-next)
(define-key *global-map* (key "C-x b") 'switch-buffer)
(define-key *global-map* (key "C-x k") 'delete-buffer)
(define-key *global-map* (key "M-l") 'set-url-new-buffer)
(define-key *global-map* (key "C-m k") 'bookmark-delete)
(define-key *global-map* (key "C-t") 'make-visible-new-buffer)
(define-key *global-map* (key "C-m u") 'bookmark-url)
(define-key *global-map* (key "C-x w") 'delete-active-buffer)
;; TODO: Rename to inspect-variable?  Wouldn't describe-variable be more familiar?
(define-key *global-map* (key "C-h v") 'variable-inspect)
(define-key *global-map* (key "C-h c") 'command-inspect)
(define-key *global-map* (key "C-o") 'load-file)
(define-key *global-map* (key "C-h s") 'start-swank)
(define-key *global-map* (key "M-x") 'execute-extended-command)
(define-key *global-map* (key "C-x 5 2") 'make-window)
(define-key *global-map* (key "C-x 5 0") 'delete-window)
(define-key *global-map* (key "C-x q") (lambda () (echo-dismiss (minibuffer *interface*))))
