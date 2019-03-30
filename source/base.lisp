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

(define-command kill ()
  "Quit Next."
  (kill-interface *interface*)
  (kill-program *port*))

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
  (start :with-platform-port-p t))

(defun initialize-port ()
  (let ((port-running nil))
    (loop while (not port-running) do
      (handler-case
          (let ((buffer (nth-value 1 (make-window))))
            (set-url-buffer (if *free-args* (car *free-args*) *start-page-url*) buffer)
            ;; We can have many URLs as positional arguments.
            (loop for url in (cdr *free-args*) do
              (let ((buffer (make-buffer)))
                (set-url-buffer url buffer)))
            (log:debug (active-buffer *interface*))
            (setf port-running t))
        (SB-BSD-SOCKETS:CONNECTION-REFUSED-ERROR ()
          (format *error-output* "Polling platform port...~%")
          (sleep *platform-port-poll-interval*)
          (setf port-running nil))
        (SB-BSD-SOCKETS:HOST-NOT-FOUND-ERROR ()
          (format *error-output* "Platform port not found.~%")
          (sleep *platform-port-poll-interval*))))))

(defun start (&key (with-platform-port-p nil))
  (map nil 'funcall *deferred-variables*)
  (when (getf *options* :init-file)
    (setf *init-file-path* (getf *options* :init-file)))
  (ensure-directories-exist (xdg-data-home))
  (map nil 'funcall *deferred-mode-initializations*)
  (ensure-directories-exist (xdg-data-home))
  (setf *default-new-buffer-mode* #'document-mode)
  ;; load the user configuration if it exists
  (if (string= (pathname-name *init-file-path*) "-")
      (progn
        (format *error-output* "Initializing from standard input...~%")
        (loop for object = (read *standard-input* nil :eof)
              until (eq object :eof)
              do (eval object)))
      ;; ignore loading errors
      (handler-case (load *init-file-path* :if-does-not-exist nil)
        (error (c)
          (format t "Error: we could not load your init file: ~a~&" c))))
  (initialize-bookmark-db)
  (initialize-history-db)
  ;; create the interface object
  (setf *interface* (make-instance 'remote-interface))
  (start-interface *interface*)
  ;; Start the port after the interface so that we don't overwrite the log when
  ;; an instance is already running.
  (when with-platform-port-p
    (run-program *port*))
  ;; initialize default state
  (setf *minibuffer* (make-instance 'minibuffer))
  (initialize-port)
  (when with-platform-port-p
    (run-loop *port*)
    (log:debug "Platform port exited")
    (kill-interface *interface*))
  t)

(setf *port* (make-instance 'port))
(set-conversion-table *port*)
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
(define-key *global-map* (key "C-h v") 'variable-inspect)
(define-key *global-map* (key "C-h c") 'command-inspect)
(define-key *global-map* (key "C-o") 'load-file)
(define-key *global-map* (key "C-h s") 'start-swank)
(define-key *global-map* (key "M-x") 'execute-extended-command)
(define-key *global-map* (key "C-x 5 2") 'make-window)
(define-key *global-map* (key "C-x 5 0") 'delete-window)
(define-key *global-map* (key "C-x q") (lambda () (echo-dismiss *minibuffer*)))
